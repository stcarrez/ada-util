-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with GNAT.Command_Line;

with AUnit.Assertions;
with AUnit.Reporter.Text;
with AUnit.Run;
with Util.Measures;
with Ada.Command_Line;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Util.Properties;
with Util.Files;
package body Util.Tests is

   use AUnit.Assertions;

   Test_Properties : Util.Properties.Manager;

   --  ------------------------------
   --  Get a path to access a test file.
   --  ------------------------------
   function Get_Path (File : String) return String is
      Dir : constant String := Get_Parameter ("test.dir", ".");
   begin
      return Dir & "/" & File;
   end Get_Path;

   --  ------------------------------
   --  Get a path to create a test file.
   --  ------------------------------
   function Get_Test_Path (File : String) return String is
      Dir : constant String := Get_Parameter ("test.result.dir", ".");
   begin
      return Dir & "/" & File;
   end Get_Test_Path;

   --  ------------------------------
   --  Get a test configuration parameter.
   --  ------------------------------
   function Get_Parameter (Name    : String;
                           Default : String := "") return String is
   begin
      return Test_Properties.Get (Name, Default);
   end Get_Parameter;

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
   procedure Assert_Equals (Expect, Value : in Integer;
                            Message : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line) is
   begin
      Assert (Condition => Expect = Value,
              Message   => Message & ": expecting '"
              & Integer'Image (Expect) & "'"
              & " value was '"
              & Integer'Image (Value) & "'",
              Source    => Source,
              Line      => Line);
   end Assert_Equals;

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
   procedure Assert_Equals (Expect, Value : in String;
                            Message   : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line) is
   begin
      Assert (Condition => Expect = Value,
              Message   => Message & ": expecting '" & Expect & "'"
              & " value was '" & Value & "'",
              Source    => Source,
              Line      => Line);
   end Assert_Equals;

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
   procedure Assert_Equals (Expect  : in String;
                            Value   : in Unbounded_String;
                            Message : in String := "Test failed";
                            Source  : String := GNAT.Source_Info.File;
                            Line    : Natural := GNAT.Source_Info.Line) is
   begin
      Assert_Equals (Expect => Expect,
                     Value  => To_String (Value),
                     Message => Message,
                     Source  => Source,
                     Line    => Line);
   end Assert_Equals;

   --  ------------------------------
   --  Check that two files are equal.  This is intended to be used by
   --  tests that create files that are then checked against patterns.
   --  ------------------------------
   procedure Assert_Equal_Files (Expect  : in String;
                                 Test    : in String;
                                 Message : in String := "Test failed";
                                 Source  : String := GNAT.Source_Info.File;
                                 Line    : Natural := GNAT.Source_Info.Line) is
      use Util.Files;

      Expect_File : Unbounded_String;
      Test_File   : Unbounded_String;
      Same        : Boolean;
   begin
      if not Ada.Directories.Exists (Expect) then
         Assert (False, "Expect file '" & Expect & "' does not exist",
                 Source => Source, Line => Line);
      end if;
      Read_File (Path => Expect,
                 Into => Expect_File);
      Read_File (Path => Test,
                 Into => Test_File);

      --  Check file sizes
      Assert_Equals (Expect => Length (Expect_File),
                     Value  => Length (Test_File),
                     Message => Message & ": Invalid file sizes",
                     Source  => Source,
                     Line    => Line);

      Same := Expect_File = Test_File;
      if Same then
         return;
      end if;
   end Assert_Equal_Files;

   --  ------------------------------
   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   --  ------------------------------
   procedure Harness (Name : in String) is
      use type AUnit.Status;
      use GNAT.Command_Line;

      function Runner is new AUnit.Run.Test_Runner_With_Status (Suite);

      Reporter : AUnit.Reporter.Text.Text_Reporter;
      Perf     : aliased Util.Measures.Measure_Set;
      Result   : AUnit.Status;
   begin
      loop
         case Getopt ("config") is
         when 'c' =>
            declare
               Name : constant String := Get_Argument;
            begin
               Test_Properties.Load_Properties (Name);
            exception
               when Ada.IO_Exceptions.Name_Error =>
                  Ada.Text_IO.Put_Line ("Cannot find configuration file: " & Name);
                  Ada.Command_Line.Set_Exit_Status (2);
                  return;
            end;

         when others =>
            exit;
         end case;
      end loop;

      declare
         S  : Util.Measures.Stamp;
      begin
         Util.Measures.Set_Current (Perf'Unchecked_Access);
         Result := Runner (Reporter, True);
         Util.Measures.Report (Perf, S, "Testsuite execution");
         Util.Measures.Write (Perf, "Test measures", Name);
      end;

      --  Program exit status reflects the testsuite result
      if Result /= AUnit.Success then
         Ada.Command_Line.Set_Exit_Status (1);
      else
         Ada.Command_Line.Set_Exit_Status (0);
      end if;
   end Harness;

end Util.Tests;
