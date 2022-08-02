-----------------------------------------------------------------------
--  util-files-rolling-tests -- Unit tests for rolling file manager
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

with Ada.Directories;
with Ada.Calendar.Formatting;
with Util.Test_Caller;
package body Util.Files.Rolling.Tests is

   package Caller is new Util.Test_Caller (Test, "Files");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Files.Rolling.Rollover (Ascending)",
                       Test_Rolling_Ascending'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Rolling.Rollover (Descending)",
                       Test_Rolling_Descending'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the Format procedure with its pattern replacement.
   --  ------------------------------
   procedure Test_Format (T : in out Test) is
      D1 : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (2011, 11, 19, 0, 0, 0);
   begin
      Util.Tests.Assert_Equals (T, "test/file10.log",
                                Format ("test/file%i.log", D1, 10),
                                "Invalid format");
      Util.Tests.Assert_Equals (T, "test-11/file-11",
                                Format ("test-%i/file-%i", D1, 11),
                                "Invalid format");
      Util.Tests.Assert_Equals (T, "test-2011-11-19.log",
                                Format ("test-%d{YYYY-MM-dd}.log", D1, 11),
                                "Invalid format");
   end Test_Format;

   --  ------------------------------
   --  Test a rolling manager configured with ascending rolling mode.
   --  ------------------------------
   procedure Test_Rolling_Ascending (T : in out Test) is
      Dir     : constant String := Util.Tests.Get_Test_Path ("rolling");
      Pat     : constant String := Compose (Dir, "rolling-%i.txt");
      Manager : File_Manager;
   begin
      --  Erase target directory to make sure it is empty for this test.
      if Ada.Directories.Exists (Dir) then
         Ada.Directories.Delete_Tree (Dir);
      end if;

      Manager.Initialize ("rolling.txt", Pat,
                          Policy   => (Kind => No_Policy),
                          Strategy => (Ascending_Strategy, 1, 10));
      for I in 1 .. 20 loop
         Write_File (Path    => Manager.Get_Current_Path,
                     Content => "Content" & Natural'Image (I));
         Manager.Rollover;
      end loop;

      for I in 1 .. 10 loop
         declare
            Name : constant String := "rolling-" & Util.Strings.Image (I) & ".txt";
            Path : constant String := Compose (Dir, Name);
            Result : Unbounded_String;
         begin
            T.Assert (Ada.Directories.Exists (Path), "File '" & Path & "' not found");
            Read_File (Path, Result);
            Util.Tests.Assert_Equals (T, "Content" & Natural'Image (I + 10),
                                      To_String (Result),
                                      "Invalid rolling" & Natural'Image (I));
         end;
      end loop;
   end Test_Rolling_Ascending;

   --  ------------------------------
   --  Test a rolling manager configured with descending rolling mode.
   --  ------------------------------
   procedure Test_Rolling_Descending (T : in out Test) is
      Dir     : constant String := Util.Tests.Get_Test_Path ("rolling");
      Pat     : constant String := Compose (Dir, "roll-desc-%i.txt");
      Manager : File_Manager;
   begin
      --  Erase target directory to make sure it is empty for this test.
      if Ada.Directories.Exists (Dir) then
         Ada.Directories.Delete_Tree (Dir);
      end if;

      Manager.Initialize ("rolling.txt", Pat,
                          Policy   => (Kind => No_Policy),
                          Strategy => (Descending_Strategy, 1, 10));
      for I in 1 .. 20 loop
         Write_File (Path    => Manager.Get_Current_Path,
                     Content => "Content" & Natural'Image (I));
         Manager.Rollover;
      end loop;

      for I in 1 .. 10 loop
         declare
            Name : constant String := "roll-desc-" & Util.Strings.Image (I) & ".txt";
            Path : constant String := Compose (Dir, Name);
            Result : Unbounded_String;
         begin
            T.Assert (Ada.Directories.Exists (Path), "File '" & Path & "' not found");
            Read_File (Path, Result);
            Util.Tests.Assert_Equals (T, "Content" & Natural'Image (20 - I + 1),
                                      To_String (Result),
                                      "Invalid rolling" & Natural'Image (I));
         end;
      end loop;
   end Test_Rolling_Descending;

end Util.Files.Rolling.Tests;
