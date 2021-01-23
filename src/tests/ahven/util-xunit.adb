-----------------------------------------------------------------------
--  util-xunit - Unit tests on top of AHven
--  Copyright (C) 2011, 2016, 2017, 2018, 2019, 2021 Stephane Carrez
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
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada.Calendar;

with Ahven.Listeners.Basic;
with Ahven.XML_Runner;
with Ahven.Text_Runner;
with Ahven.AStrings;

with Util.Tests;
with Util.Strings;
package body Util.XUnit is

   function Image (Time : in Duration) return String;
   procedure Report_XML_Summary (Path    : in String;
                                 Result  : in Ahven.Results.Result_Collection;
                                 Time    : in Duration);

   --  ------------------------------
   --  Build a message from a string (Adaptation for AUnit API).
   --  ------------------------------
   function Format (S : in String) return Message_String is
   begin
      return S;
   end Format;

   --  ------------------------------
   --  Build a message with the source and line number.
   --  ------------------------------
   function Build_Message (Message   : in String;
                           Source    : in String;
                           Line      : in Natural) return String is
      L : constant String := Natural'Image (Line);
   begin
      return Source & ":" & L (2 .. L'Last) & ": " & Message;
   end Build_Message;

   procedure Run_Test_Case (T : in out Ahven.Framework.Test_Case'Class);

   procedure Run_Test_Case (T : in out Ahven.Framework.Test_Case'Class) is
   begin
      Test_Case'Class (T).Run_Test;
   end Run_Test_Case;

   overriding
   procedure Initialize (T : in out Test_Case) is
   begin
      Ahven.Framework.Add_Test_Routine (T, Run_Test_Case'Access, "Test case");
   end Initialize;

   --  ------------------------------
   --  Return the name of the test case.
   --  ------------------------------
   overriding
   function Get_Name (T : Test_Case) return String is
   begin
      return Test_Case'Class (T).Name;
   end Get_Name;

   --  maybe_overriding
   procedure Assert (T         : in Test_Case;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : in String := GNAT.Source_Info.File;
                     Line      : in Natural := GNAT.Source_Info.Line) is
      pragma Unreferenced (T);
   begin
      Ahven.Assert (Condition => Condition,
                    Message   => Build_Message (Message => Message,
                                                Source  => Source,
                                                Line    => Line));
   end Assert;

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
   procedure Assert (T         : in Test;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : String := GNAT.Source_Info.File;
                     Line      : Natural := GNAT.Source_Info.Line) is
      pragma Unreferenced (T);
   begin
      Ahven.Assert (Condition => Condition,
                    Message   => Build_Message (Message => Message,
                                                Source  => Source,
                                                Line    => Line));
   end Assert;

   First_Test : Test_Object_Access := null;

   --  ------------------------------
   --  Register a test object in the test suite.
   --  ------------------------------
   procedure Register (T : in Test_Object_Access) is
   begin
      T.Next := First_Test;
      First_Test := T;
   end Register;

   --  ------------------------------
   --  Report passes, skips, failures, and errors from the result collection.
   --  ------------------------------
   procedure Report_Results (Result  : in Ahven.Results.Result_Collection;
                             Label   : in String;
                             Time    : in Duration) is
      T_Count : constant Integer := Ahven.Results.Test_Count (Result);
      F_Count : constant Integer := Ahven.Results.Failure_Count (Result);
      S_Count : constant Integer := Ahven.Results.Skipped_Count (Result);
      E_Count : constant Integer := Ahven.Results.Error_Count (Result);
   begin
      if F_Count > 0 then
         Ahven.Text_Runner.Print_Failures (Result, 0);
      end if;
      if E_Count > 0 then
         Ahven.Text_Runner.Print_Errors (Result, 0);
      end if;
      Ada.Text_IO.Put_Line (Label
                            & "Tests run:" & Integer'Image (T_Count - S_Count)
                            & ", Failures:" & Integer'Image (F_Count)
                            & ", Errors:" & Integer'Image (E_Count)
                            & ", Skipped:" & Integer'Image (S_Count)
                            & ", Time elapsed:" & Duration'Image (Time));

   end Report_Results;

   function Image (Time : in Duration) return String is
      Result : constant String := Duration'Image (Time);
   begin
      if Result (Result'First) = ' ' then
         return Result (Result'First + 1 .. Result'Last);
      else
         return Result;
      end if;
   end Image;

   --  ------------------------------
   --  Write a XML summary report in the JUnit format so that the result file
   --  can be used by Jenkins performance plugin.
   --  ------------------------------
   procedure Report_XML_Summary (Path    : in String;
                                 Result  : in Ahven.Results.Result_Collection;
                                 Time    : in Duration) is
      use Ahven.Results;
      File : Ada.Text_IO.File_Type;
      Group : Result_Collection_Cursor;
      Iter : Result_Collection_Cursor;
      Test : Result_Collection_Access;
   begin
      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Path);
      Ada.Text_IO.Put_Line (File, "<?xml version='1.0'?>");
      Ada.Text_IO.Put (File, "<testsuite ");
      Ada.Text_IO.Put (File, "errors='"
                       & Util.Strings.Image (Error_Count (Result)) & "' ");
      Ada.Text_IO.Put (File, "failures='"
                       & Util.Strings.Image (Failure_Count (Result)) & "' ");
      Ada.Text_IO.Put (File, "tests='"
                       & Util.Strings.Image (Test_Count (Result)) & "' ");
      Ada.Text_IO.Put (File, "time='" & Image (Time) & "' ");
      Ada.Text_IO.Put (File, "name='");
      Ada.Text_IO.Put (File, Ahven.AStrings.To_String (Get_Test_Name (Result)));
      Ada.Text_IO.Put_Line (File, "'>");
      Group := First_Child (Result);
      while Is_Valid (Group) loop
         Iter := First_Child (Data (Group).all);
         while Is_Valid (Iter) loop
            Test := Data (Iter);
            Ada.Text_IO.Put (File, "<testcase name='");
            Ada.Text_IO.Put (File, Ahven.AStrings.To_String (Get_Test_Name (Test.all)));
            Ada.Text_IO.Put (File, "' errors='");
            Ada.Text_IO.Put (File, Util.Strings.Image (Error_Count (Test.all)));
            Ada.Text_IO.Put (File, "' failures='");
            Ada.Text_IO.Put (File, Util.Strings.Image (Failure_Count (Test.all)));
            Ada.Text_IO.Put (File, "' tests='");
            Ada.Text_IO.Put (File, Util.Strings.Image (Test_Count (Test.all)));
            Ada.Text_IO.Put (File, "' time='");
            Ada.Text_IO.Put (File, Image (Get_Execution_Time (Test.all)));
            Ada.Text_IO.Put_Line (File, "'/>");
            Iter := Next (Iter);
         end loop;
         Group := Next (Group);
      end loop;
      Ada.Text_IO.Put_Line (File, "</testsuite>");
      Ada.Text_IO.Close (File);
   end Report_XML_Summary;

   --  ------------------------------
   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   --  ------------------------------
   procedure Harness (Output : in String;
                      XML    : in Boolean;
                      Label  : in String;
                      Result : out Status) is

      use Ahven.Listeners.Basic;
      use Ahven.Framework;
      use Ahven.Results;
      use type Ada.Calendar.Time;

      Tests    : constant Access_Test_Suite := Suite;
      T        : Test_Object_Access := First_Test;
      Listener : Ahven.Listeners.Basic.Basic_Listener;
      Timeout  : constant Test_Duration := Test_Duration (Util.Tests.Get_Test_Timeout ("all"));
      Out_Dir  : constant String := Util.Tests.Get_Test_Path ("");
      Start    : Ada.Calendar.Time;
      Dt       : Duration;
   begin
      while T /= null loop
         Ahven.Framework.Add_Static_Test (Tests.all, T.Test.all);
         T := T.Next;
      end loop;
      Set_Output_Capture (Listener, True);
      if not Ada.Directories.Exists (Out_Dir) then
         Ada.Directories.Create_Path (Out_Dir);
      end if;

      Ahven.Framework.Set_Logging (Util.Tests.Verbose);
      Start := Ada.Calendar.Clock;
      Ahven.Framework.Execute (Tests.all, Listener, Timeout);
      Dt := Ada.Calendar.Clock - Start;
      Report_Results (Listener.Main_Result, Label, Dt);

      Ahven.XML_Runner.Report_Results (Listener.Main_Result, Out_Dir);

      if (Error_Count (Listener.Main_Result) > 0) or
        (Failure_Count (Listener.Main_Result) > 0)
      then
         Result := Failure;
      else
         Result := Success;
      end if;

      if XML then
         Report_XML_Summary (Output, Listener.Main_Result, Dt);
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("Cannot create file");
         Result := Failure;

   end Harness;

end Util.XUnit;
