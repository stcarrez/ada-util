-----------------------------------------------------------------------
--  util-files-rolling -- Rolling file manager
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with GNAT.Regpat;
with Util.Dates.Simple_Format;
package body Util.Files.Rolling is

   use type Ada.Calendar.Time;
   use type Ada.Directories.File_Size;

   --  ------------------------------
   --  Format the file pattern with the date and index to produce a file path.
   --  ------------------------------
   function Format (Pattern : in String;
                    Date    : in Ada.Calendar.Time;
                    Index   : in Natural) return String is
      Pos    : Natural := Pattern'First;
      Result : Unbounded_String;
      Info   : Util.Dates.Date_Record;
      First  : Natural;
   begin
      Util.Dates.Split (Info, Date);
      while Pos <= Pattern'Last loop
         if Pattern (Pos) /= '%' or else Pos = Pattern'Last then
            Ada.Strings.Unbounded.Append (Result, Pattern (Pos));
            Pos := Pos + 1;
         elsif Pattern (Pos + 1) = 'i' then
            Ada.Strings.Unbounded.Append (Result, Util.Strings.Image (Index));
            Pos := Pos + 2;
         elsif Pos + 3 < Pattern'Last and then Pattern (Pos + 1) = 'd'
           and then Pattern (Pos + 2) = '{'
         then
            Pos := Pos + 3;
            First := Pos;
            while Pos <= Pattern'Last and Pattern (Pos) /= '}' loop
               Pos := Pos + 1;
            end loop;
            Append (Result, Util.Dates.Simple_Format (Pattern (First .. Pos - 1), Date));
            Pos := Pos + 1;
         else
            Ada.Strings.Unbounded.Append (Result, Pattern (Pos));
            Pos := Pos + 1;
         end if;
      end loop;
      return To_String (Result);
   end Format;

   --  ------------------------------
   --  Initialize the file manager to roll the file refered by `Path` by using
   --  the pattern defined in `Pattern`.
   --  ------------------------------
   procedure Initialize (Manager  : in out File_Manager;
                         Path     : in String;
                         Pattern  : in String;
                         Policy   : in Policy_Type;
                         Strategy : in Strategy_Type) is
   begin
      Manager.Deadline := Ada.Calendar.Clock;
      Manager.File_Path := To_Unbounded_String (Path);
      Manager.Pattern := To_Unbounded_String (Pattern);
      Manager.Cur_Index := 0;
      Manager.Policy := Policy.Kind;
      case Policy.Kind is
         when Size_Policy =>
            Manager.Max_Size := Policy.Size;

         when Time_Policy =>
            Manager.Interval := Policy.Interval;

         when others =>
            null;
      end case;
      Manager.Strategy := Strategy.Kind;
      case Strategy.Kind is
         when Ascending_Strategy | Descending_Strategy =>
            Manager.Min_Index := Strategy.Min_Index;
            Manager.Max_Index := Strategy.Max_Index;
            Manager.Max_Files := Manager.Max_Index - Manager.Min_Index;

         when Direct_Strategy =>
            Manager.Max_Files := Strategy.Max_Files;

      end case;
      Manager.Rollover;
   end Initialize;

   --  ------------------------------
   --  Get the current path (it may or may not exist).
   --  ------------------------------
   function Get_Current_Path (Manager : in File_Manager) return String is
   begin
      return To_String (Manager.File_Path);
   end Get_Current_Path;

   --  ------------------------------
   --  Check if a rollover is necessary based on the rolling strategy.
   --  ------------------------------
   function Is_Rollover_Necessary (Manager : in File_Manager) return Boolean is
   begin
      case Manager.Policy is
         when No_Policy =>
            return False;

         when Time_Policy =>
            return Manager.Deadline < Ada.Calendar.Clock;

         when Size_Policy =>
            declare
               Path : constant String := To_String (Manager.File_Path);
            begin
               if not Ada.Directories.Exists (Path) then
                  return False;
               end if;
               return Ada.Directories.Size (Path) >= Manager.Max_Size;

            exception
               when Ada.Directories.Name_Error =>
                  --  Even if we check for file existence, an exception could
                  --  be raised if the file is moved/deleted.
                  return False;
            end;

      end case;
   end Is_Rollover_Necessary;

   --  Perform a rollover according to the strategy that was configured.
   procedure Rollover (Manager : in out File_Manager) is
   begin
      case (Manager.Strategy) is
         when Ascending_Strategy =>
            Manager.Rollover_Ascending;

         when Descending_Strategy =>
            Manager.Rollover_Descending;

         when Direct_Strategy =>
            Manager.Rollover_Direct;

      end case;
   end Rollover;

   procedure Rollover_Ascending (Manager : in out File_Manager) is
      Old   : constant String := Manager.Get_Current_Path;
      Path  : constant String := Format (Pattern => To_String (Manager.Pattern),
                                         Date    => Manager.Deadline,
                                         Index   => Manager.Cur_Index);
      Dir   : constant String := Ada.Directories.Containing_Directory (Path);
      Names : Util.Strings.Vectors.Vector;
      First : Natural := 0;
      Last  : Natural := 0;
   begin
      if Ada.Directories.Exists (Dir) then
         Manager.Elligible_Files (Dir, Names, First, Last);

         while Natural (Names.Length) > Manager.Max_Files loop
            begin
               Ada.Directories.Delete_File (Compose (Dir, Names.First_Element));

            exception
               when Ada.Directories.Name_Error =>
                  null;
            end;
            Names.Delete_First;
         end loop;
      end if;

      Manager.Cur_Index := Last + 1;

      --  Too many files, rename old ones.
      if Manager.Cur_Index > Manager.Max_Index then
         Manager.Cur_Index := Manager.Max_Index - Natural (Names.Length);
         for Name of Names loop
            Manager.Rename (Compose (Dir, Name));
            Manager.Cur_Index := Manager.Cur_Index + 1;
         end loop;
      end if;

      Manager.Rename (Old);
   end Rollover_Ascending;

   procedure Rename (Manager : in File_Manager;
                     Old     : in String) is
      Path  : constant String := Format (Pattern => To_String (Manager.Pattern),
                                         Date    => Manager.Deadline,
                                         Index   => Manager.Cur_Index);
      Dir   : constant String := Ada.Directories.Containing_Directory (Path);
   begin
      if Ada.Directories.Exists (Old) then
         if not Ada.Directories.Exists (Dir) then
            Ada.Directories.Create_Path (Dir);
         end if;

         Util.Files.Rename (Old_Name => Old,
                            New_Name => Path);
      end if;
   end Rename;

   procedure Rollover_Descending (Manager : in out File_Manager) is
      Old   : constant String := Manager.Get_Current_Path;
      Path  : constant String := Format (Pattern => To_String (Manager.Pattern),
                                         Date    => Manager.Deadline,
                                         Index   => Manager.Cur_Index);
      Dir   : constant String := Ada.Directories.Containing_Directory (Path);
      Names : Util.Strings.Vectors.Vector;
      First : Natural := 0;
      Last  : Natural := 0;
   begin
      if Ada.Directories.Exists (Dir) then
         Manager.Elligible_Files (Dir, Names, First, Last);

         while Natural (Names.Length) > Manager.Max_Files loop
            begin
               Ada.Directories.Delete_File (Compose (Dir, Names.Last_Element));

            exception
               when Ada.Directories.Name_Error =>
                  null;
            end;
            Names.Delete_Last;
         end loop;
      end if;

      Manager.Cur_Index := First;

      if not Names.Is_Empty then
         Manager.Cur_Index := Manager.Min_Index + Natural (Names.Length);
         for Name of reverse Names loop
            Manager.Rename (Compose (Dir, Name));
            Manager.Cur_Index := Manager.Cur_Index - 1;
         end loop;
      end if;

      Manager.Rename (Old);
   end Rollover_Descending;

   procedure Rollover_Direct (Manager : in out File_Manager) is
   begin
      null;
   end Rollover_Direct;

   --  ------------------------------
   --  Get the regex pattern to identify a file that must be purged.
   --  The default is to extract the file pattern part of the file manager pattern.
   --  ------------------------------
   function Get_Purge_Pattern (Manager : in File_Manager) return String is
      Full_Pat : constant String := To_String (Manager.Pattern);
      Name_Pat : constant String := Ada.Directories.Simple_Name (Full_Pat);
      Pos      : Natural := Name_Pat'First;
      Result   : Unbounded_String;
      Found    : Boolean := False;
   begin
      while Pos <= Name_Pat'Last loop
         if Name_Pat (Pos) = '%' and then Pos + 1 <= Name_Pat'Last
           and then Name_Pat (Pos + 1) = 'i'
         then
            if not Found then
               Append (Result, "([0-9]+)");
               Found := True;
            else
               Append (Result, "[0-9]+");
            end if;
            Pos := Pos + 2;
         else
            Append (Result, Name_Pat (Pos));
            Pos := Pos + 1;
         end if;
      end loop;
      return To_String (Result);
   end Get_Purge_Pattern;

   --  ------------------------------
   --  Find the files that are elligible to purge in the given directory.
   --  ------------------------------
   procedure Elligible_Files (Manager     : in out File_Manager;
                              Path        : in String;
                              Names       : in out Util.Strings.Vectors.Vector;
                              First_Index : out Natural;
                              Last_Index  : out Natural) is
      function Get_Index (Name : in String) return Natural;
      function Compare (Left, Right : in String) return Boolean;

      Search_Filter : constant Ada.Directories.Filter_Type
        := (Ada.Directories.Ordinary_File => True,
            Ada.Directories.Directory     => False,
            Ada.Directories.Special_File  => False);
      Pattern  : constant String := Manager.Get_Purge_Pattern;
      Regex    : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (Pattern);
      Glob1    : constant String := Util.Strings.Replace (Pattern, "([0-9]+)", "*", False);
      Glob2    : constant String := Util.Strings.Replace (Glob1, "[0-9]+", "*", False);
      Search   : Ada.Directories.Search_Type;
      Ent      : Ada.Directories.Directory_Entry_Type;

      function Get_Index (Name : in String) return Natural is
         Matches : GNAT.Regpat.Match_Array (0 .. 1);
      begin
         if GNAT.Regpat.Match (Regex, Name) then
            GNAT.Regpat.Match (Regex, Name, Matches);
            return Natural'Value (Name (Matches (1).First .. Matches (1).Last));
         else
            return 0;
         end if;

      exception
         when others =>
            return 0;
      end Get_Index;

      function Compare (Left, Right : in String) return Boolean is
         Left_Index  : constant Natural := Get_Index (Left);
         Right_Index : constant Natural := Get_Index (Right);
      begin
         return Left_Index < Right_Index;
      end Compare;

      package Sort_Names is new Util.Strings.Vectors.Generic_Sorting (Compare);
   begin
      Ada.Directories.Start_Search (Search,
                                    Directory => Path,
                                    Pattern   => Glob2,
                                    Filter    => Search_Filter);
      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Ent);
         declare
            Name    : constant String := Ada.Directories.Simple_Name (Ent);
         begin
            Names.Append (Name);
         end;
      end loop;
      Sort_Names.Sort (Names);
      if not Names.Is_Empty then
         First_Index := Get_Index (Names.First_Element);
         Last_Index := Get_Index (Names.Last_Element);
      else
         First_Index := Manager.Min_Index;
         Last_Index := Manager.Min_Index;
      end if;
   end Elligible_Files;

end Util.Files.Rolling;
