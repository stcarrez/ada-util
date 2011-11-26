-----------------------------------------------------------------------
--  util-xunit - Unit tests on top of AUnit
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

with AUnit;
with AUnit.Simple_Test_Cases;
with AUnit.Test_Suites;
with AUnit.Test_Fixtures;
with AUnit.Assertions;
with Ada.Strings.Unbounded;
with Ada.Calendar;

with GNAT.Source_Info;

with Util.Assertions;

--  The <b>Util.XUnit</b> package exposes a common package definition used by the Ada testutil
--  library.  It is intended to hide the details of the AUnit implementation.
package Util.XUnit is

   use Ada.Strings.Unbounded;
   use AUnit.Test_Suites;

   subtype Status is AUnit.Status;

   Success : constant Status := AUnit.Success;
   Failure : constant Status := AUnit.Failure;

   subtype Message_String is AUnit.Message_String;
   subtype Test_Case is AUnit.Simple_Test_Cases.Test_Case;
   subtype Test is AUnit.Test_Fixtures.Test_Fixture;
   subtype Test_Suite is AUnit.Test_Suites.Test_Suite;
   subtype Access_Test_Suite is AUnit.Test_Suites.Access_Test_Suite;

   function Format (S : in String) return Message_String renames AUnit.Format;

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  Check that two files are equal.  This is intended to be used by
   --  tests that create files that are then checked against patterns.
   procedure Assert_Equal_Files (T       : in AUnit.Assertions.Test'Class;
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

   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   generic
      with function Suite return Access_Test_Suite;
   procedure Harness (Output : in Ada.Strings.Unbounded.Unbounded_String;
                      XML    : in Boolean;
                      Result : out Status);

end Util.XUnit;
