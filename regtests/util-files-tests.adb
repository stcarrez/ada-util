-----------------------------------------------------------------------
--  files.tests -- Unit tests for files
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Util.Test_Caller;
with Util.Tests;
package body Util.Files.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Files.Read_File",
                       Test_Read_File'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Read_File (missing)",
                       Test_Read_File'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Read_File (truncate)",
                       Test_Read_File'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Write_File",
                       Test_Write_File'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Iterate_Path",
                       Test_Iterate_Path'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Find_File_Path",
                       Test_Find_File_Path'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Compose_Path",
                       Test_Compose_Path'Access);
   end Add_Tests;

   --  Test reading a file into a string
   --  Reads this ada source file and checks we have read it correctly
   procedure Test_Read_File (T : in out Test) is
      Result : Unbounded_String;
   begin
      Read_File (Path => "regtests/util-files-tests.adb", Into => Result);
      T.Assert (Index (Result, "Util.Files.Tests") > 0,
                "Content returned by Read_File is not correct");
      T.Assert (Index (Result, "end Util.Files.Tests;") > 0,
                "Content returned by Read_File is not correct");
   end Test_Read_File;

   procedure Test_Read_File_Missing (T : in out Test) is
      Result : Unbounded_String;

      pragma Unreferenced (Result);
   begin
      Read_File (Path => "regtests/files-test--util.adb", Into => Result);
      T.Assert (False, "No exception raised");
   exception
      when others =>
         null;
   end Test_Read_File_Missing;

   procedure Test_Read_File_Truncate (T : in out Test) is
      Result : Unbounded_String;
   begin
      Read_File (Path => "regtests/util-files-tests.adb", Into => Result,
                 Max_Size => 50);
      Assert_Equals (T, Length (Result), 50,
                     "Read_File did not truncate correctly");
      T.Assert (Index (Result, "Apache License") > 0,
                "Content returned by Read_File is not correct");
   end Test_Read_File_Truncate;

   --  Check writing a file
   procedure Test_Write_File (T : in out Test) is
      Path   : constant String := Util.Tests.Get_Test_Path ("test-write.txt");
      Content : constant String := "Testing Util.Files.Write_File" & ASCII.LF;
      Result : Unbounded_String;
   begin
      Write_File (Path => Path, Content => Content);
      Read_File (Path => Path, Into => Result);
      Assert_Equals (T, To_String (Result), Content,
                     "Invalid content written or read");
   end Test_Write_File;

   --  Check Find_File_Path
   procedure Test_Find_File_Path (T : in out Test) is
      Dir   : constant String := Util.Tests.Get_Path ("regtests");
      Paths : constant String := ".;" & Dir;
   begin
      declare
         P : constant String := Util.Files.Find_File_Path ("test.properties", Paths);
      begin
         Assert_Equals (T, Dir & "/test.properties", P,
                        "Invalid path returned");
      end;
      Assert_Equals (T, "blablabla.properties",
                     Util.Files.Find_File_Path ("blablabla.properties", Paths));
   end Test_Find_File_Path;

   --  ------------------------------
   --  Check Iterate_Path
   --  ------------------------------
   procedure Test_Iterate_Path (T : in out Test) is

      Last : Unbounded_String;

      procedure Check_Path (Dir : in String;
                            Done : out Boolean) is
      begin
         if Dir = "a" or Dir = "bc" or Dir = "de" then
            Done := False;
         else
            Done := True;
            Last := To_Unbounded_String (Dir);
         end if;
      end Check_Path;

   begin
      Iterate_Path ("a;bc;de;f", Check_Path'Access);
      Assert_Equals (T, "f", Last, "Invalid last path");

      Iterate_Path ("de;bc;de;b", Check_Path'Access);
      Assert_Equals (T, "b", Last, "Invalid last path");

   end Test_Iterate_Path;

   --  ------------------------------
   --  Test the Compose_Path operation
   --  ------------------------------
   procedure Test_Compose_Path (T : in out Test) is
   begin
      Assert_Equals (T, "/usr/bin;/usr/local/bin;/usr/bin",
                     Compose_Path ("/usr;/usr/local;/usr", "bin"),
                     "Invalid path composition");
   end Test_Compose_Path;


end Util.Files.Tests;
