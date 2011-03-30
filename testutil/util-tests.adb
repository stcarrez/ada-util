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

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Run;
with Ada.Command_Line;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada.Calendar.Formatting;

with Util.Strings;
with Util.Measures;
with Util.Files;
with Util.Log.Loggers;
package body Util.Tests is

   use AUnit.Assertions;

   Test_Properties : Util.Properties.Manager;

   --  When a test uses external test files to match a result against a well
   --  defined content, it can be difficult to maintain those external files.
   --  The <b>Assert_Equal_Files</b> can automatically maintain the reference
   --  file by updating it with the lastest test result.
   --
   --  Of course, using this mode means the test does not validate anything.
   Update_Test_Files : Boolean := False;

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
   --  Get the test configuration properties.
   --  ------------------------------
   function Get_Properties return Util.Properties.Manager is
   begin
      return Test_Properties;
   end Get_Properties;

   --  ------------------------------
   --  Get a new unique string
   --  ------------------------------
   function Get_Uuid return String is
      Time  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Year  : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day   : Ada.Calendar.Day_Number;
      T     : Ada.Calendar.Day_Duration;
      V     : Long_Long_Integer;
   begin
      Ada.Calendar.Split (Date    => Time,
                          Year    => Year,
                          Month   => Month,
                          Day     => Day,
                          Seconds => T);
      V := (Long_Long_Integer (Year) * 365 * 24 * 3600 * 1000)
        + (Long_Long_Integer (Month) * 31 * 24 * 3600 * 1000)
        + (Long_Long_Integer (Day) * 24 * 3600 * 1000)
        + (Long_Long_Integer (T * 1000));
      return "U" & Util.Strings.Image (V);
   end Get_Uuid;

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
--     procedure Assert_Equals (T       : in AUnit.Assertions.Test'Class;
--                              Expect, Value : in Integer;
--                              Message : in String := "Test failed";
--                              Source    : String := GNAT.Source_Info.File;
--                              Line      : Natural := GNAT.Source_Info.Line) is
--     begin
--        T.Assert (Condition => Expect = Value,
--                  Message   => Message & ": expecting '"
--                  & Integer'Image (Expect) & "'"
--                  & " value was '"
--                  & Integer'Image (Value) & "'",
--                  Source    => Source,
--                  Line      => Line);
--     end Assert_Equals;

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
   procedure Assert_Equals (T         : in AUnit.Assertions.Test'Class;
                            Expect, Value : in Ada.Calendar.Time;
                            Message   : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line) is
      use Ada.Calendar.Formatting;
      use Ada.Calendar;
   begin
      T.Assert (Condition => Image (Expect) = Image (Value),
                Message   => Message & ": expecting '" & Image (Expect) & "'"
                & " value was '" & Image (Value) & "'",
                Source    => Source,
                Line      => Line);
   end Assert_Equals;

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
   procedure Assert_Equals (T         : in AUnit.Assertions.Test'Class;
                            Expect, Value : in String;
                            Message   : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line) is
   begin
      T.Assert (Condition => Expect = Value,
                Message   => Message & ": expecting '" & Expect & "'"
                & " value was '" & Value & "'",
                Source    => Source,
                Line      => Line);
   end Assert_Equals;

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
   procedure Assert_Equals (T       : in AUnit.Assertions.Test'Class;
                            Expect  : in String;
                            Value   : in Unbounded_String;
                            Message : in String := "Test failed";
                            Source  : String := GNAT.Source_Info.File;
                            Line    : Natural := GNAT.Source_Info.Line) is
   begin
      Assert_Equals (T      => T,
                     Expect => Expect,
                     Value  => To_String (Value),
                     Message => Message,
                     Source  => Source,
                     Line    => Line);
   end Assert_Equals;

   --  ------------------------------
   --  Check that two files are equal.  This is intended to be used by
   --  tests that create files that are then checked against patterns.
   --  ------------------------------
   procedure Assert_Equal_Files (T       : in AUnit.Assertions.Test'Class;
                                 Expect  : in String;
                                 Test    : in String;
                                 Message : in String := "Test failed";
                                 Source  : String := GNAT.Source_Info.File;
                                 Line    : Natural := GNAT.Source_Info.Line) is
      use Util.Files;

      Expect_File : Unbounded_String;
      Test_File   : Unbounded_String;
      Same        : Boolean;
   begin
      begin
         if not Ada.Directories.Exists (Expect) then
            T.Assert (False, "Expect file '" & Expect & "' does not exist",
                      Source => Source, Line => Line);
         end if;
         Read_File (Path => Expect,
                    Into => Expect_File);
         Read_File (Path => Test,
                    Into => Test_File);

      exception
         when others =>
            if Update_Test_Files then
               Ada.Directories.Copy_File (Source_Name => Test,
                                          Target_Name => Expect);
            end if;
      end;

      --  Check file sizes
      Assert_Equals (T       => T,
                     Expect  => Length (Expect_File),
                     Value   => Length (Test_File),
                     Message => Message & ": Invalid file sizes",
                     Source  => Source,
                     Line    => Line);

      Same := Expect_File = Test_File;
      if Same then
         return;
      end if;
   end Assert_Equal_Files;

   --  ------------------------------
   --  Default initialization procedure.
   --  ------------------------------
   procedure Initialize_Test (Props : in Util.Properties.Manager) is
   begin
      null;
   end Initialize_Test;

   --  ------------------------------
   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   --  ------------------------------
   procedure Harness (Name : in String) is
      use type AUnit.Status;
      use GNAT.Command_Line;
      use Ada.Text_IO;

      function Runner is new AUnit.Run.Test_Runner_With_Status (Suite);

      procedure Help;

      procedure Help is
      begin
         Put_Line ("Test harness: " & Name);
         Put ("Usage: harness [-config file.properties] ");
         Put_Line ("[-update]");
         Put_Line ("-config file   Specify a test configuration file");
         Put_Line ("-update        Update the test reference files if a file");
         Put_Line ("               is missing or the test generates another output");
         Put_Line ("               (See Asset_Equals_File)");
         Ada.Command_Line.Set_Exit_Status (2);
      end Help;

      Reporter : AUnit.Reporter.Text.Text_Reporter;
      Perf     : aliased Util.Measures.Measure_Set;
      Result   : AUnit.Status;
   begin
      loop
         case Getopt ("hu c: config: update help") is
            when ASCII.NUL =>
               exit;

            when 'c' =>
               declare
                  Name : constant String := Parameter;
               begin
                  Test_Properties.Load_Properties (Name);
               exception
                  when Ada.IO_Exceptions.Name_Error =>
                     Ada.Text_IO.Put_Line ("Cannot find configuration file: " & Name);
                     Ada.Command_Line.Set_Exit_Status (2);
                     return;
               end;

            when 'u' =>
               Update_Test_Files := True;

            when others =>
               Help;
               return;
         end case;
      end loop;

      --  Initialization is optional.  Get the log configuration by reading the property
      --  file 'samples/log4j.properties'.  The 'log.util' logger will use a DEBUG level
      --  and write the message in 'result.log'.
      Util.Log.Loggers.Initialize (Test_Properties);

      Initialize (Test_Properties);

      declare
         S  : Util.Measures.Stamp;
         O  : AUnit.Options.AUnit_Options := AUnit.Options.Default_Options;
      begin
         O.Global_Timer := True;
         Util.Measures.Set_Current (Perf'Unchecked_Access);
         Result := Runner (Reporter, O);
         Util.Measures.Report (Perf, S, "Testsuite execution");
         Util.Measures.Write (Perf, "Test measures", Name);
      end;

      --  Program exit status reflects the testsuite result
      if Result /= AUnit.Success then
         Ada.Command_Line.Set_Exit_Status (1);
      else
         Ada.Command_Line.Set_Exit_Status (0);
      end if;

   exception
      when Invalid_Switch =>
         Put_Line ("Invalid Switch " & Full_Switch);
         Help;
         return;

      when Invalid_Parameter =>
         Put_Line ("No parameter for " & Full_Switch);
         Help;
         return;
   end Harness;

end Util.Tests;
