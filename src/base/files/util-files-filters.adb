-- --------------------------------------------------------------------
--  util-files-filters -- Path filters
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
package body Util.Files.Filters is

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Element_Type,
                                     Name   => Element_Access);

   procedure Set_Value (Node  : in Pattern_Access;
                        Value : in Element_Type);

   function Match_Recursive_Pattern (Recursive : in Pattern_Access;
                                     Path      : in String) return Pattern_Access;
   function Match_Pattern (Root : in Pattern_Access;
                           Path : in String) return Pattern_Access;
   function Get_Result (Pattern : in Pattern_Access) return Filter_Result;

   function Get_Value (Result : in Filter_Result) return Element_Type is
   begin
      return Result.Pattern.Value.all;
   end Get_Value;

   function Is_Only_Directory (Result : in Filter_Result) return Boolean is
   begin
      return Result.Pattern.Dir_Only;
   end Is_Only_Directory;

   --  ------------------------------
   --  Add a new pattern and associate it with the given value.
   --  ------------------------------
   procedure Insert (Filter    : in out Filter_Type;
                     Pattern   : in String;
                     Recursive : in Boolean;
                     Value     : in Element_Type) is
   begin
      if Recursive then
         Insert (Filter.Recursive, Pattern, Value);
      else
         Insert (Filter.Root, Pattern, Value);
      end if;
   end Insert;

   procedure Set_Value (Node  : in Pattern_Access;
                        Value : in Element_Type) is
   begin
      Free (Node.Value);
      Node.Value := new Element_Type '(Value);
   end Set_Value;

   procedure Insert (Root    : in out Pattern_Access;
                     Pattern : in String;
                     Value   : in Element_Type) is

      function Find_Child (Node : Pattern_Access;
                           Name : String) return Pattern_Access;

      function Is_Wildcard (Name : String)
                            return Boolean is (Name = "*");

      function Is_Multi_Wildcard (Name : String)
                                  return Boolean is (Name = "**");

      function Find_Child (Node : Pattern_Access;
                           Name : String) return Pattern_Access is
      begin
         if Node /= null then
            return Find_Pattern (Node.Child, Name);
         else
            return Find_Pattern (Root, Name);
         end if;
      end Find_Child;

      First : Positive := Pattern'First;
      Exact : Boolean;
      Pos   : Natural;
      C     : Character;
      Node  : Pattern_Access;
      Previous : Pattern_Access;
   begin
      while First <= Pattern'Last loop
         Exact := True;
         while First <= Pattern'Last and then Pattern (First) = '/' loop
            First := First + 1;
         end loop;
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
               Node := Find_Child (Previous, Pattern (First .. Pos - 1));
               if Node = null then
                  Node := new Name_Pattern_Type
                    '(Len  => Pos - First,
                      Name => Pattern (First .. Pos - 1),
                      Negative => False,
                      Dir_Only => True,
                      others => <>);
                  if Previous = null then
                     Node.Next := Root;
                     Root := Node;
                  else
                     Node.Next := Previous.Child;
                     Previous.Child := Node;
                  end if;
               elsif Node.Child = null then
                  --  This is a leaf node and we are adding a directory node.
                  --  Add a leaf node with empty name for it.
                  Node.Child := new Name_Pattern_Type
                    '(Len  => 0,
                      Name => "",
                      Exclude => Node.Exclude,
                      Dir_Only => False,
                      others => <>);
                  Node.Dir_Only := True;
               end if;
               Set_Value (Node, Value);
               return;
            elsif Pos < Pattern'Last then
               Node := Find_Child (Previous, Pattern (First .. Pos - 1));
               if Node = null then
                  Node := new Name_Pattern_Type
                    '(Len  => Pos - First,
                      Name => Pattern (First .. Pos - 1),
                      Exclude => False,
                      Dir_Only => True,
                      others => <>);
                  if Previous = null then
                     Node.Next := Root;
                     Root := Node;
                  else
                     Node.Next := Previous.Child;
                     Previous.Child := Node;
                  end if;
               elsif Node.Child = null then
                  --  This is a leaf node and we are adding a directory node.
                  --  Add a leaf node with empty name for it.
                  Node.Child := new Name_Pattern_Type
                    '(Len  => 0,
                      Name => "",
                      Exclude => Node.Exclude,
                      Dir_Only => False,
                      others => <>);
                  Node.Dir_Only := True;
               end if;
               Previous := Node;
            else
               Node := Find_Child (Previous, Pattern (First .. Pattern'Last));
               if Node = null then
                  Node := new Name_Pattern_Type
                    '(Len  => Pos - First,
                      Name => Pattern (First .. Pos - 1),
                      Dir_Only => False,
                      others => <>);
                  if Previous = null then
                     Node.Next := Root;
                     Root := Node;
                  else
                     Node.Next := Previous.Child;
                     Previous.Child := Node;
                  end if;
               --  else
               --   if Node.Child /= null then
               --      Previous := Node;
               --      Node := Find_Pattern (Previous.Child, "");
               --      if Node = null then
               --         Node := new Name_Pattern_Type
               --           '(Len  => 0,
               --             Name => "",
               --             Dir_Only => False,
               --             Next => Previous.Child,
               --             others => <>);
               --         Previous.Child := Node;
               --      end if;
               --   end if;
               --   Node.Dir_Only := False;
               end if;
               Set_Value (Node, Value);
               return;
            end if;
         else
            if Is_Multi_Wildcard (Pattern (First .. Pos - 1)) then
               Node := new Name_Pattern_Type
                 '(Len  => 2,
                   Name => "**",
                   Multi_Wildcard => True,
                   Dir_Only => True,
                   others => <>);
            elsif Is_Wildcard (Pattern (First .. Pos - 1)) then
               Node := new Name_Pattern_Type
                 '(Len  => 1,
                   Name => "*",
                   Multi_Wildcard => False,
                   Dir_Only => True,
                   others => <>);
            else
               Node := new Regex_Pattern_Type
                 '(Regex => GNAT.Regexp.Compile (Pattern (First .. Pos - 1),
                                                 Glob => True),
                   Multi_Wildcard => False,
                   Dir_Only => False,
                   others => <>);
            end if;
            if Previous = null then
               Node.Next := Root;
               Root := Node;
            else
               Node.Next := Previous.Child;
               Previous.Child := Node;
            end if;
            Previous := Node;
            if Pos >= Pattern'Last then
               Set_Value (Node, Value);
            end if;
         end if;
         First := Pos + 1;
      end loop;
   end Insert;

   --  ------------------------------
   --  Check if a path matches the included or excluded patterns.
   --  Return the last node that matches the path.
   --  ------------------------------
   function Match_Recursive_Pattern (Recursive : in Pattern_Access;
                                     Path      : in String) return Pattern_Access is
      Last   : constant Natural := Path'Last;
      Pos    : Natural := Path'First;
      Result : Pattern_Access;
   begin
      while Pos <= Last loop
         Result := Match_Pattern (Recursive, Path (Pos .. Path'Last));
         if Result /= null then
            return Result;
         end if;
         Pos := Util.Strings.Index (Path, '/', Pos);
         exit when Pos = 0;
         Pos := Pos + 1;
      end loop;
      return null;
   end Match_Recursive_Pattern;

   --  ------------------------------
   --  Check if a path matches the included or excluded patterns.
   --  ------------------------------
   function Match_Pattern (Root : in Pattern_Access;
                           Path : in String) return Pattern_Access is
      Last  : constant Natural := Path'Last;
      First : Natural := Path'First;
      Pos   : Natural := First;
      Node  : Pattern_Access := Root;
      Next  : Pattern_Access;
   begin
      --  Step 1: find a match with an absolute pattern.
      while Pos <= Last and then Node /= null loop
         First := Pos;
         Pos := Util.Strings.Index (Path, '/', First);
         if Pos > 0 then
            loop
               Next := Match (Node, Path (First .. Pos - 1));
               if Next /= null and then Next.Dir_Only then
                  Node := Next.Child;
                  Pos := Pos + 1;
                  exit;
               end if;
               Node := Node.Next;
               exit when Node = null;
            end loop;
         else
            loop
               Next := Match (Node, Path (First .. Last));
               if Next /= null then
                  --  We found the matching node, if this is not the leaf
                  --  look for a child with an empty name.
                  return Next;
               end if;
               Node := Node.Next;
               exit when Node = null;
            end loop;
         end if;
      end loop;
      return null;
   end Match_Pattern;

   function Get_Result (Pattern : in Pattern_Access) return Filter_Result is
   begin
      if Pattern = null then
         return Filter_Result'(Match => Not_Found, Pattern => null);
      elsif Pattern.Value = null then
         return Filter_Result'(Match => No_Value, Pattern => Pattern);
      else
         return Filter_Result '(Match   => Found,
                                Pattern => Pattern);
      end if;
   end Get_Result;

   --  ------------------------------
   --  Check if a path matches the included or excluded patterns.
   --  ------------------------------
   function Match (Filter : in Filter_Type;
                   Path   : in String) return Filter_Result is
      Result : Pattern_Access;
   begin
      --  Step 1: find a match with an absolute pattern.
      if Filter.Root /= null then
         Result := Match_Pattern (Filter.Root, Path);
         if Result /= null then
            return Get_Result (Result);
         end if;
      end if;
      if Filter.Recursive = null then
         return Filter_Result'(Match => Not_Found, Pattern => null);
      end if;

      --  Step 2: find a match on a recursive pattern.
      Result := Match_Recursive_Pattern (Filter.Recursive, Path);
      return Get_Result (Result);
   end Match;

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
         Free (Tree.Value);
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

   function Create (Filter : in Filter_Type'Class) return Filter_Context_Type is
   begin
      return Result : Filter_Context_Type do
         Result.Filter.Current := Filter.Root;
         Result.Filter.Recursive := Filter.Recursive;
      end return;
   end Create;

   function Create (Filter  : in Filter_Type'Class;
                    Context : in Filter_Context_Type) return Filter_Context_Type is
   begin
      return Result : Filter_Context_Type do
         Result.Filter.Local := Filter.Root;
         Result.Filter.Local_Recursive := Filter.Recursive;
         Result.Filter.Previous := Context.Filter'Unchecked_Access;
      end return;
   end Create;

   function Create (Context : in Filter_Context_Type;
                    Match   : in Filter_Result) return Filter_Context_Type is
   begin
      return Result : Filter_Context_Type do
         Result.Filter.Current := Match.Pattern;
         Result.Filter.Previous := Context.Filter'Unchecked_Access;
      end return;
   end Create;

   function Find_Pattern (Node : in Pattern_Access;
                          Name : in String) return Pattern_Access is
      N : Pattern_Access := Node;
   begin
      while N /= null loop
         if N.Is_Pattern (Name) then
            return N;
         end if;
         N := N.Next;
      end loop;
      return null;
   end Find_Pattern;

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

   function Match_Sibling (Filter : access constant Filter_Info_Type;
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
      F : access constant Filter_Info_Type;
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

   --  Check if a path matches the included or excluded patterns.
   function Match (Filter : in Filter_Context_Type;
                   Path   : in String) return Filter_Result is
      Pattern : Pattern_Access;
   begin
      Pattern := Match (Filter.Filter, Path);
      return Get_Result (Pattern);
   end Match;

end Util.Files.Filters;
