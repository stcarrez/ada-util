-- --------------------------------------------------------------------
--  util-files-walk -- Walk directory trees
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Exceptions;
with Util.Log.Loggers;
package body Util.Files.Walk is

   Log : constant Util.Log.Loggers.Logger :=
     Util.Log.Loggers.Create ("Util.Files.Walk");

   --  ------------------------------
   --  Add a new pattern to include files or directories in the walk.
   --  ------------------------------
   procedure Include (Filter  : in out Filter_Type;
                      Pattern : in String) is
   begin
      if Pattern (Pattern'First) = '/' then
         Filter.Insert (Pattern   => Pattern (Pattern'First + 1 .. Pattern'Last),
                        Recursive => False,
                        Value     => Included);
      else
         Filter.Insert (Pattern   => Pattern,
                        Recursive => True,
                        Value     => Included);
      end if;
   end Include;

   --  ------------------------------
   --  Add a new pattern to exclude (ignore) files or directories in the walk.
   --  ------------------------------
   procedure Exclude (Filter  : in out Filter_Type;
                      Pattern : in String) is
   begin
      if Pattern (Pattern'First) = '/' then
         Filter.Insert (Pattern   => Pattern (Pattern'First + 1 .. Pattern'Last),
                        Recursive => False,
                        Value     => Excluded);
      else
         Filter.Insert (Pattern   => Pattern,
                        Recursive => True,
                        Value     => Excluded);
      end if;
   end Exclude;

   --  ------------------------------
   --  Check if a path matches the included or excluded patterns.
   --  ------------------------------
   function Match (Filter : in Filter_Type;
                   Path   : in String) return Filter_Mode is
      Result : constant Path_Filter.Filter_Result := Filter.Match (Path);
   begin
      if Result.Match = Path_Filter.Not_Found then
         return Not_Found;
      elsif Result.Match = Path_Filter.No_Value then
         return Not_Found;
      else
         return Path_Filter.Get_Value (Result);
      end if;
   end Match;

   --  ------------------------------
   --  Get the path of a file that can be read to get a list of files to ignore
   --  in the given directory (ie, .gitignore).
   --  ------------------------------
   function Get_Ignore_Path (Walker : Walker_Type;
                             Path   : String) return String is
      pragma Unreferenced (Walker, Path);
   begin
      return "";
   end Get_Ignore_Path;

   --  ------------------------------
   --  Returns true if the path corresponds to a root path for a project:
   --  The default returns True.
   --  ------------------------------
   function Is_Root (Walker : in Walker_Type;
                     Path   : in String) return Boolean is
   begin
      return True;
   end Is_Root;

   --  ------------------------------
   --  Find the root directory of a project knowing a path of a file or
   --  directory of that project.  Move up to parent directories until
   --  a path returns true when `Is_Root` is called.
   --  ------------------------------
   function Find_Root (Walker : in Walker_Type;
                       Path   : in String) return String is
      Real_Path : constant String := Util.Files.Realpath (Path);
      Pos       : Natural := Real_Path'Last;
   begin
      while not Walker.Is_Root (Real_Path (Real_Path'First .. Pos)) loop
         declare
            Parent : constant String :=
              Ada.Directories.Containing_Directory (Real_Path (Real_Path'First .. Pos));
         begin
            if Parent'Length = 0 then
               return Real_Path;
            end if;
            Pos := Real_Path'First + Parent'Length - 1;
         end;
      end loop;
      return Real_Path (Real_Path'First .. Pos);
   end Find_Root;

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
                   Path   : in String;
                   Filter : in Filter_Type'Class) is
      Dir         : constant String := Util.Files.Realpath (Path);
      Root        : constant String := Walker_Type'Class (Walker).Find_Root (Dir);
      Rel_Path    : constant String := Util.Files.Get_Relative_Path (Root, Dir);
      Dir_Context : constant Filter_Context_Type := Filter.Create;
   begin
      Log.Debug ("Scanning {0}", Path);

      if Rel_Path /= "." then
         Walker_Type'Class (Walker).Scan_Subdir_For_Ignore (Root, Path, Rel_Path, Dir_Context);
      else
         Walker.Scan_Subdir (Path, Dir_Context, Path_Filter.NO_MATCH);
      end if;
   end Scan;

   procedure Scan_Subdir_For_Ignore (Walker    : in out Walker_Type;
                                     Path      : in String;
                                     Scan_Path : in String;
                                     Rel_Path  : in String;
                                     Filter    : in Filter_Context_Type) is
      Sep : constant Natural := Path_Component_Position (Rel_Path);
      Child_Dir   : constant String := Compose (Path, Rel_Path (Rel_Path'First .. Sep));
      Ignore_File : constant String := Walker_Type'Class (Walker).Get_Ignore_Path (Path);
      Local_Filter : Filter_Type;
   begin
      if Ignore_File'Length > 0 and then Ada.Directories.Exists (Ignore_File) then
         Walker_Type'Class (Walker).Load_Ignore (Ignore_File, Local_Filter);
         declare
            Dir_Context  : constant Filter_Context_Type
              := Local_Filter.Create (Filter);
         begin
            if Sep < Rel_Path'Last then
               Walker_Type'Class (Walker).Scan_Subdir_For_Ignore
                 (Child_Dir, Scan_Path,
                  Rel_Path (Sep + 1 .. Rel_Path'Last),
                  Dir_Context);
            else
               --  Reached end of relative path to load ignore files, scan from the Scan_Path now.
               Walker_Type'Class (Walker).Scan_Directory (Scan_Path, Dir_Context);
            end if;
         end;
      else
         declare
            Dir_Context : constant Filter_Context_Type
              := Path_Filter.Create (Filter);
         begin
            if Sep < Rel_Path'Last then
               Walker_Type'Class (Walker).Scan_Subdir_For_Ignore
                 (Child_Dir, Scan_Path,
                  Rel_Path (Sep + 1 .. Rel_Path'Last),
                  Dir_Context);
            else
               --  Reached end of relative path to load ignore files, scan from the Scan_Path now.
               Walker_Type'Class (Walker).Scan_Directory (Scan_Path, Dir_Context);
            end if;
         end;
      end if;
   end Scan_Subdir_For_Ignore;

   procedure Scan_Subdir (Walker : in out Walker_Type;
                          Path   : in String;
                          Filter : in Filter_Context_Type;
                          Match  : in Filter_Result) is
      Ignore_File : constant String
        := Walker_Type'Class (Walker).Get_Ignore_Path (Path);
   begin
      Log.Debug ("Scanning {0}", Path);

      if Ignore_File'Length > 0
        and then Ada.Directories.Exists (Ignore_File)
      then
         declare
            Local_Filter : Filter_Type;
         begin
            Walker_Type'Class (Walker).Load_Ignore (Ignore_File, Local_Filter);
            declare
               Dir_Context  : constant Filter_Context_Type
                 := Local_Filter.Create (Filter);
            begin
               Walker_Type'Class (Walker).Scan_Directory (Path,
                                                          Dir_Context);
            end;
         end;
      elsif Match.Match /= Not_Found then
         declare
            Dir_Context : constant Filter_Context_Type
              := Path_Filter.Create (Filter, Match);
         begin
            Walker_Type'Class (Walker).Scan_Directory (Path,
                                                       Dir_Context);
         end;
      else
         declare
            Dir_Context : constant Filter_Context_Type
              := Path_Filter.Create (Filter);
         begin
            Walker_Type'Class (Walker).Scan_Directory (Path,
                                                       Dir_Context);
         end;
      end if;
   end Scan_Subdir;

   --  ------------------------------
   --  Load a series of lines that contains a list of files to ignore.
   --  The `Reader` procedure is called with a `Process` procedure that is
   --  expected to be called for each line which comes from the ignore
   --  file (such as the .gitignore file).  The `Process` procedure handles
   --  the interpretation of ignore patterns as defined by `.gitignore`
   --  and it updates the `Filter` accordingly.
   --  ------------------------------
   procedure Load_Ignore (Filter : in out Filter_Type'Class;
                          Label  : in String;
                          Reader : not null access
                              procedure (Process : not null access
                                   procedure (Line : in String))) is
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
               begin
                  if Negative then
                     Filter.Include (Line (Line'First + 1 .. Last));
                  else
                     Filter.Exclude (Line (Line'First .. Last));
                  end if;

               exception
                  when E : GNAT.Regexp.Error_In_Regexp =>
                     Log.Error ("{0}:{1}: Invalid regular expression: {2}",
                                Label,
                                Util.Strings.Image (Line_Number),
                                Ada.Exceptions.Exception_Message (E));
               end;
            end if;
         end if;
      end Process;
   begin
      Reader (Process'Access);
   end Load_Ignore;

   --  ------------------------------
   --  Load the file that contains a list of files to ignore.  The default
   --  implementation reads patterns as defined in `.gitignore` files.
   --  ------------------------------
   procedure Load_Ignore (Filter : in out Filter_Type'Class;
                          Path   : String) is
      procedure Reader (Process : not null access procedure (Line : in String));

      procedure Reader (Process : not null access procedure (Line : in String)) is
      begin
         Util.Files.Read_File (Path, Process);
      end Reader;
   begin
      Log.Debug ("Loading ignore file {0}", Path);

      Filter.Load_Ignore (Path, Reader'Access);
   end Load_Ignore;

   --  ------------------------------
   --  Load the file that contains a list of files to ignore.  The default
   --  implementation reads patterns as defined in `.gitignore` files.
   --  ------------------------------
   procedure Load_Ignore (Walker : in out Walker_Type;
                          Path   : String;
                          Filter : in out Filter_Type'Class) is
      pragma Unreferenced (Walker);
   begin
      Load_Ignore (Filter, Path);
   end Load_Ignore;

   function Is_File_Excluded (Result : Filter_Result) return Boolean
     is (Result.Match = Found and then Get_Value (Result) = Excluded
         and then not Is_Only_Directory (Result));

   function Is_Directory_Excluded (Result : Filter_Result) return Boolean
     is ((Result.Match = Found and then Get_Value (Result) = Excluded)
          or else (Result.Match = Found and then Is_Only_Directory (Result)));

   --  ------------------------------
   --  Called when a directory is found during a directory tree walk.
   --  The default implementation scans the directory for files and directories
   --  matching the filter.  It can be overriden to implement specific
   --  behaviors.
   --  ------------------------------
   procedure Scan_Directory (Walker : in out Walker_Type;
                             Path   : in String;
                             Filter : in Filter_Context_Type) is
      package AD renames Ada.Directories;
      use type AD.File_Kind;

      Dir_Filter  : constant AD.Filter_Type := (AD.Ordinary_File => True,
                                                AD.Directory     => True,
                                                others           => False);
      Ent    : AD.Directory_Entry_Type;
      Search : AD.Search_Type;
   begin
      Log.Debug ("Scanning {0}", Path);

      AD.Start_Search (Search, Directory => Path,
                       Pattern => "*", Filter => Dir_Filter);
      while AD.More_Entries (Search) loop
         AD.Get_Next_Entry (Search, Ent);
         declare
            Name   : constant String := AD.Simple_Name (Ent);
         begin
            if Name /= "." and then Name /= ".." then
               declare
                  Full_Path : constant String := Util.Files.Compose (Path, Name);
                  Kind      : constant AD.File_Kind := AD.Kind (Full_Path);
                  Result    : constant Filter_Result := Match (Filter, Name);
               begin
                  --  Log.Debug ("{0} => {1}",
                  --             AD.Full_Name (Ent), Result.Match'Image);
                  case Kind is
                     when AD.Ordinary_File =>
                        if not Is_File_Excluded (Result) then
                           Walker_Type'Class (Walker).Scan_File (Full_Path);
                        end if;

                     when AD.Directory =>
                        if not Is_Directory_Excluded (Result) then
                           Walker_Type'Class (Walker).Scan_Subdir (Full_Path, Filter, Result);
                        end if;

                     when others =>
                        null;
                  end case;
               end;
            end if;
         end;
      end loop;
   end Scan_Directory;

end Util.Files.Walk;
