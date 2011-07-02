-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
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

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;
with Ada.Strings.Unbounded;
with Ada.Calendar;

with GNAT.Source_Info;

with Util.Properties;
with Util.Assertions;
package Util.Tests is

   use Ada.Strings.Unbounded;
   use AUnit.Test_Suites;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  Get a path to access a test file.
   function Get_Path (File : String) return String;

   --  Get a path to create a test file.
   function Get_Test_Path (File : String) return String;

   --  Get a test configuration parameter.
   function Get_Parameter (Name    : String;
                           Default : String := "") return String;

   --  Get the test configuration properties.
   function Get_Properties return Util.Properties.Manager;

   --  Get a new unique string
   function Get_Uuid return String;

   --  Check that two files are equal.  This is intended to be used by
   --  tests that create files that are then checked against patterns.
   procedure Assert_Equal_Files (T       : in Test'Class;
                                 Expect  : in String;
                                 Test    : in String;
                                 Message : in String := "Test failed";
                                 Source  : String := GNAT.Source_Info.File;
                                 Line    : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches what we expect.
   procedure Assert_Equals is new Assertions.Assert_Equals_T (Value_Type => Integer);
   procedure Assert_Equals is new Assertions.Assert_Equals_T (Value_Type => Character);

   --  Check that the value matches what we expect.
   procedure Assert (T         : in Test'Class;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : String := GNAT.Source_Info.File;
                     Line      : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches what we expect.
   procedure Assert_Equals (T         : in Test'Class;
                            Expect, Value : in Ada.Calendar.Time;
                            Message   : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches what we expect.
   procedure Assert_Equals (T         : in Test'Class;
                            Expect, Value : in String;
                            Message   : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches what we expect.
   procedure Assert_Equals (T       : in Test'Class;
                            Expect  : in String;
                            Value   : in Unbounded_String;
                            Message : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line);

   --  Default initialization procedure.
   procedure Initialize_Test (Props : in Util.Properties.Manager);

   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   generic
      with function Suite return Access_Test_Suite;
      with procedure Initialize (Props : in Util.Properties.Manager) is Initialize_Test;
   procedure Harness (Name : in String);

end Util.Tests;
