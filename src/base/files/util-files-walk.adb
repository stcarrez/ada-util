-- --------------------------------------------------------------------
--  util-files-walk -- Walk directory trees
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Util.Log.Loggers;
package body Util.Files.Walk is

   Log : constant Util.Log.Loggers.Logger :=
     Util.Log.Loggers.Create ("Util.Files.Walk");

   procedure Insert (Root    : in out Pattern_Access;
                     Pattern : String;
                     Exclude : Boolean);

   --  ------------------------------
   --  Add a new pattern to include files or directories in the walk.
   --  ------------------------------
   procedure Include (Filter  : in out Filter_Type;
                      Pattern : String) is
   begin
      if Pattern (Pattern'First) = '/' then
         Insert (Filter.Root,
                 Pattern (Pattern'First + 1 .. Pattern'Last),
                 False);
      else
         Insert (Filter.Recursive, Pattern, False);
      end if;
   end Include;

   --  ------------------------------
   --  Add a new pattern to exclude (ignore) files or directories in the walk.
   --  ------------------------------
   procedure Exclude (Filter  : in out Filter_Type;
                      Pattern : String) is
   begin
      if Pattern (Pattern'First) = '/' then
         Insert (Filter.Root,
                 Pattern (Pattern'First + 1 .. Pattern'Last),
                 True);
      else
         Insert (Filter.Recursive, Pattern, True);
      end if;
   end Exclude;

   overriding
   procedure Finalize (Filter : in out Filter_Type) is
      procedure Release (Tree : in out Pattern_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Pattern_Type'Class,
                                        Name   => Pattern_Access);

      --  Release siblings and children.
      procedure Release (Tree : in out Pattern_Access) is
      begin
         if Tree.Child /= null then
            Release (Tree.Child);
         end if;
         while Tree.Next /= null loop
            declare
               Next : Pattern_Access := Tree.Next;
            begin
               Tree.Next := Next.Next;
               Next.Next := null;
               Release (Next);
            end;
         end loop;
         Free (Tree);
      end Release;
   begin
      if Filter.Root /= null then
         Release (Filter.Root);
      end if;
      if Filter.Recursive /= null then
         Release (Filter.Recursive);
      end if;
   end Finalize;

   procedure Insert (Root    : in out Pattern_Access;
                     Pattern : String;
                     Exclude : Boolean) is

      function Is_Wildcard (Name : String)
                            return Boolean is (Name = "*");

      function Is_Multi_Wildcard (Name : String)
                                  return Boolean is (Name = "**");

      First : Positive := Pattern'First;
      Exact : Boolean;
      Pos   : Natural;
      C     : Character;
      Node  : Pattern_Access;
      Previous : Pattern_Access;
   begin
      while First <= Pattern'Last loop
         Exact := True;
         Pos := First;
         while Pos <= Pattern'Last loop
            C := Pattern (Pos);
            exit when C = '/';
            if C in '(' | ')' | '[' | ']' | '\' | '*' | '?' then
               Exact := False;
            end if;
            Pos := Pos + 1;
         end loop;
         if Exact then
            if Pos = Pattern'Last and then Pattern (Pos) = '/' then
               Node := new Name_Pattern_Type
                 '(Len  => Pos - First,
                   Name => Pattern (First .. Pos - 1),
                   Exclude  => Exclude,
                   Negative => False,
                   Dir_Only => True,
                   others => <>);
            elsif Pos < Pattern'Last then
               Node := new Name_Pattern_Type
                 '(Len  => Pos - First,
                   Name => Pattern (First .. Pos - 1),
                   Exclude => False,
                   Dir_Only => True,
                   others => <>);
            else
               Node := new Name_Pattern_Type
                 '(Len  => Pos - First,
                   Name => Pattern (First .. Pos - 1),
                   Exclude => Exclude,
                   Dir_Only => False,
                   others => <>);
            end if;
            if First = Pattern'First then
               Node.Next := Root;
               Root := Node;
               Previous := Node;
            else
               Previous.Child := Node;
               Previous := Node;
            end if;
         else
            if Is_Multi_Wildcard (Pattern (First .. Pos - 1)) then
               Node := new Name_Pattern_Type
                 '(Len  => 2,
                   Name => "**",
                   Exclude => Exclude,
                   Multi_Wildcard => True,
                   Dir_Only => True,
                   others => <>);
            elsif Is_Wildcard (Pattern (First .. Pos - 1)) then
               Node := new Name_Pattern_Type
                 '(Len  => 1,
                   Name => "*",
                   Exclude => Exclude,
                   Multi_Wildcard => False,
                   Dir_Only => True,
                   others => <>);
            else
               Node := new Regex_Pattern_Type
                 '(Regex => GNAT.Regexp.Compile (Pattern (First .. Pos - 1),
                                                 Glob => True),
                   Exclude => Exclude,
                   Multi_Wildcard => False,
                   Dir_Only => False,
                   others => <>);
            end if;
            if First = Pattern'First then
               Node.Next := Root;
               Root := Node;
               Previous := Node;
            else
               Previous.Child := Node;
               Previous := Node;
            end if;
         end if;
         First := Pos + 1;
      end loop;
   end Insert;

   function Match (Pattern : Pattern_Access;
                   Name    : String) return Pattern_Access is
      P : Pattern_Access := Pattern;
   begin
      while P /= null loop
         if P.Is_Matched (Name) then
            return P;
         end if;
         P := P.Next;
      end loop;
      return null;
   end Match;

   function Match_Sibling (Filter : access Filter_Info_Type;
                           Name   : String) return Pattern_Access is
      P : Pattern_Access := Filter.Current;
   begin
      if P = null then
         return null;
      end if;
      while not P.Multi_Wildcard loop
         P := P.Next;
         if P = null then
            return null;
         end if;
      end loop;
      if P.Child /= null then
         return Match (P.Child, Name);
      end if;
      return P;
   end Match_Sibling;

   function Match (Filter : Filter_Info_Type;
                   Name   : String) return Pattern_Access is
      F : access Filter_Info_Type;
      Match1, Match2, Match3, Match4, Match5, Match6 : Pattern_Access := null;
   begin
      if Filter.Local /= null then
         Match1 := Match (Filter.Local, Name);
         if Match1 /= null and then not Match1.Exclude then
            return Match1;
         end if;
      end if;
      if Filter.Current /= null then
         Match2 := Match (Filter.Current, Name);
         if Match2 /= null and then not Match2.Exclude then
            return Match2;
         end if;
         if Filter.Current.Child /= null then
            Match6 := Match (Filter.Current.Child, Name);
            if Match6 /= null and then not Match6.Exclude then
               return Match6;
            end if;
         end if;
      end if;
      if Filter.Local_Recursive /= null then
         Match3 := Match (Filter.Local_Recursive, Name);
         if Match3 /= null and then not Match3.Exclude then
            return Match3;
         end if;
      end if;
      if Filter.Recursive /= null then
         Match4 := Match (Filter.Recursive, Name);
         if Match4 /= null and then not Match4.Exclude then
            return Match4;
         end if;
      end if;
      if Filter.Previous /= null then
         F := Filter.Previous;
         while F /= null loop
            Match5 := Match_Sibling (F, Name);
            if Match5 /= null and then not Match5.Exclude then
               return Match5;
            end if;
            if F.Local_Recursive /= null then
               Match5 := Match (F.Local_Recursive, Name);
               if Match5 /= null and then not Match5.Exclude then
                  return Match5;
               end if;
               if Match6 = null then
                  Match6 := Match5;
               end if;
            end if;
            if F.Recursive /= null then
               Match5 := Match (F.Recursive, Name);
               if Match5 /= null and then not Match5.Exclude then
                  return Match5;
               end if;
            end if;
            if Match6 = null then
               Match6 := Match5;
            end if;
            F := F.Previous;
         end loop;
      end if;
      return (if Match1 /= null then Match1
              elsif Match2 /= null then Match2
              elsif Match3 /= null then Match3
              else Match6);
   end Match;

   --  ------------------------------
   --  Scan the directory tree given by the path for files and sub-directories
   --  matching the filters:
   --  * it calls `Get_Ignore_Path` to get an optional path of files to read
   --    for patterns to ignore,
   --  * if that file exist, it calls `Load_Ignore` in a local filter and
   --    loads the patterns in the filter.
   --  * it scans the directory for files and sub-directories matching the
   --    filters (either defined by the loaded ignored files) or by the main
   --    filter.
   --  * when files are found and are not excluded, it calls `Scan_File`,
   --  * when sub-directories are found and are not excluded, it calls
   --    `Scan_Directory`.
   --  ------------------------------
   procedure Scan (Walker : in out Walker_Type;
                   Path   : String;
                   Filter : Filter_Type'Class) is
      Dir_Filter  : aliased Filter_Info_Type;
      Dir_Context : Filter_Context_Type;
   begin
      Log.Debug ("Scanning {0}", Path);

      Dir_Filter.Current := Filter.Root;
      Dir_Filter.Recursive := Filter.Recursive;
      Dir_Context.Filter := Dir_Filter'Unchecked_Access;
      Walker.Scan_Subdir (Path, Dir_Context);
   end Scan;

   procedure Scan_Subdir (Walker  : in out Walker_Type;
                          Path    : String;
                          Filter  : Filter_Context_Type) is
      Ignore_File : constant String
        := Walker_Type'Class (Walker).Get_Ignore_Path (Path);
      Dir_Filter  : aliased Filter_Info_Type;
      Dir_Context : Filter_Context_Type;
   begin
      Log.Debug ("Scanning {0}", Path);

      Dir_Filter.Previous := Filter.Filter;
      Dir_Filter.Current := Filter.Pattern;
      Dir_Context.Filter := Dir_Filter'Unchecked_Access;
      if Ignore_File'Length > 0
        and then Ada.Directories.Exists (Ignore_File)
      then
         declare
            Local_Filter : Filter_Type;
         begin
            Walker_Type'Class (Walker).Load_Ignore (Ignore_File, Local_Filter);
            Dir_Filter.Local := Local_Filter.Root;
            Dir_Filter.Local_Recursive := Local_Filter.Recursive;
            Walker_Type'Class (Walker).Scan_Directory (Path,
                                                       Dir_Context);
         end;
      else
         Walker_Type'Class (Walker).Scan_Directory (Path,
                                                    Dir_Context);
      end if;
   end Scan_Subdir;

   --  ------------------------------
   --  Load the file that contains a list of files to ignore.  The default
   --  implementation reads patterns as defined in `.gitignore` files.
   --  ------------------------------
   procedure Load_Ignore (Walker : in out Walker_Type;
                          Path   : String;
                          Filter : in out Filter_Type'Class) is
      pragma Unreferenced (Walker);

      procedure Process (Line : String);

      Line_Number : Natural := 0;
      procedure Process (Line : String) is
         Last     : Natural;
         Negative : Boolean;
      begin
         Line_Number := Line_Number + 1;
         if Line'Length > 0 and then Line (Line'First) /= '#' then
            Last := Line'Last;
            while Last >= Line'First loop
               exit when Line (Last) /= ' ';
               exit when Last - 1 >= Line'First and then Line (Last - 1) = '\';
               Last := Last - 1;
            end loop;
            if Last >= Line'First then
               Negative := Line (Line'First) = '!';
               if Negative then
                  Filter.Include (Line (Line'First + 1 .. Last));
               else
                  Filter.Exclude (Line (Line'First .. Last));
               end if;
            end if;
         end if;
      end Process;
   begin
      Log.Debug ("Loading ignore file {0}", Path);

      Util.Files.Read_File (Path, Process'Access);
   end Load_Ignore;

   --  ------------------------------
   --  Called when a directory is found during a directory tree walk.
   --  The default implementation scans the directory for files and directories
   --  matching the filter.  It can be overriden to implement specific
   --  behaviors.
   --  ------------------------------
   procedure Scan_Directory (Walker : in out Walker_Type;
                             Path   : String;
                             Filter : Filter_Context_Type) is
      package AD renames Ada.Directories;
      use type AD.File_Kind;

      Dir_Filter  : constant AD.Filter_Type := (AD.Ordinary_File => True,
                                                AD.Directory     => True,
                                                others        => False);
      Ent    : AD.Directory_Entry_Type;
      Search : AD.Search_Type;
      Result : Filter_Context_Type;
   begin
      Log.Debug ("Scanning {0}", Path);

      Result.Filter := Filter.Filter;
      AD.Start_Search (Search, Directory => Path,
                       Pattern => "*", Filter => Dir_Filter);
      while AD.More_Entries (Search) loop
         AD.Get_Next_Entry (Search, Ent);
         declare
            Name   : constant String := AD.Simple_Name (Ent);
         begin
            if Name /= "." and then Name /= ".." then
               Result.Pattern := Match (Filter.Filter.all, Name);
               if Result.Pattern = null or else not Result.Pattern.Exclude
                 or else Result.Pattern.Dir_Only
               then
                  declare
                     Full_Path : constant String := AD.Full_Name (Ent);
                     Kind      : constant AD.File_Kind := AD.Kind (Full_Path);
                  begin
                     if Kind /= AD.Directory
                       and then (Result.Pattern = null or else not Result.Pattern.Exclude)
                     then
                        Walker_Type'Class (Walker).Scan_File (Full_Path);
                     elsif Kind = AD.Directory
                       and then (Result.Pattern = null or else not Result.Pattern.Exclude)
                     then
                        Walker_Type'Class (Walker).Scan_Subdir (Full_Path, Result);
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;
   end Scan_Directory;

end Util.Files.Walk;
