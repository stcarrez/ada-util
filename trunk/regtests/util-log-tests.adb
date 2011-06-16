-----------------------------------------------------------------------
--  log.tests -- Unit tests for loggers
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

with Ada.Strings.Fixed;
with Ada.Directories;

with Util.Test_Caller;

with Util.Log;
with Util.Log.Loggers;
with Util.Properties;
with Util.Measures;
package body Util.Log.Tests is

   use Util;

   Log : constant Loggers.Logger := Loggers.Create ("util.log.test");

   procedure Test_Log (T : in out Test) is
      pragma Unreferenced (T);

      L : Loggers.Logger := Loggers.Create ("util.log.test.debug");
   begin
      L.Set_Level (DEBUG_LEVEL);
      Log.Info ("My log message");
      Log.Error ("My error message");
      Log.Debug ("A debug message Not printed");

      L.Info ("An info message");
      L.Debug ("A debug message on logger 'L'");
   end Test_Log;

   --  Test configuration and creation of file
   procedure Test_File_Appender (T : in out Test) is
      pragma Unreferenced (T);

      Props : Util.Properties.Manager;
   begin
      Props.Set ("log4j.appender.test", "File");
      Props.Set ("log4j.appender.test.File", "test.log");
      Props.Set ("log4j.logger.util.log.test.file", "DEBUG,test");
      Util.Log.Loggers.Initialize (Props);

      declare
         L : constant Loggers.Logger := Loggers.Create ("util.log.test.file");
      begin
         L.Debug ("Writing a debug message");
         L.Debug ("{0}: {1}", "Parameter", "Value");
      end;
   end Test_File_Appender;

   procedure Test_Log_Perf (T : in out Test) is
      pragma Unreferenced (T);

      Props : Util.Properties.Manager;
   begin
      Props.Set ("log4j.appender.test", "File");
      Props.Set ("log4j.appender.test.File", "test.log");
      Props.Set ("log4j.logger.util.log.test.perf", "DEBUG,test");
      Util.Log.Loggers.Initialize (Props);

      for I in 1 .. 1000 loop
         declare
            S : Util.Measures.Stamp;
         begin
            Util.Measures.Report (S, "Util.Measures.Report");
         end;
      end loop;

      declare
         L : Loggers.Logger := Loggers.Create ("util.log.test.perf");
         S : Util.Measures.Stamp;
      begin
         L.Set_Level (DEBUG_LEVEL);
         for I in 1 .. 1000 loop
            L.Info ("My log message: {0}: {1}", "A message",
                    "A second parameter");
         end loop;
         Util.Measures.Report (S, "1000 Log.Info message (output)");

         L.Set_Level (INFO_LEVEL);
         for I in 1 .. 10_000 loop
            L.Debug ("My log message: {0}: {1}", "A message",
                     "A second parameter");
         end loop;
         Util.Measures.Report (S, "10000 Log.Debug message (no output)");
      end;
   end Test_Log_Perf;

   --  Test appending the log on several log files
   procedure Test_List_Appender (T : in out Test) is
      use Ada.Strings;
      use Ada.Directories;
      Props : Util.Properties.Manager;
   begin
      for I in 1 .. 10 loop
         declare
            Id   : constant String := Fixed.Trim (Integer'Image (I), Both);
            Name : constant String := "log4j.appender.test" & Id;
         begin
            Props.Set (Name, "File");
            Props.Set (Name & ".File", "test" & Id & ".log");
            Props.Set (Name & ".layout", "date-level-message");
            if I > 5 then
               Props.Set (Name & ".level", "INFO");
            end if;
         end;
      end loop;
      Props.Set ("log4j.rootCategory", "DEBUG, test.log");
      Props.Set ("log4j.logger.util.log.test.file",
                 "DEBUG,test4,test1 , test2,test3, test4, test5 ,  test6 , test7,test8,");
      Util.Log.Loggers.Initialize (Props);

      declare
         L : constant Loggers.Logger := Loggers.Create ("util.log.test.file");
      begin
         L.Debug ("Writing a debug message");
         L.Debug ("{0}: {1}", "Parameter", "Value");
      end;

      --  Check that we have non empty log files (up to test8.log).
      for I in 1 .. 8 loop
         declare
            Id   : constant String := Fixed.Trim (Integer'Image (I), Both);
            Path : constant String := "test" & Id & ".log";
         begin
            T.Assert (Ada.Directories.Exists (Path), "Log file " & Path & " not found");
            if I > 5 then
               T.Assert (Ada.Directories.Size (Path) < 100, "Log file " & Path & " should be empty");
            else
               T.Assert (Ada.Directories.Size (Path) > 100, "Log file " & Path & " is empty");
            end if;
         end;
      end loop;
   end Test_List_Appender;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Info",
                       Test_Log'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Debug",
                       Test_Log'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Set_Level",
                       Test_Log'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Appenders.File_Appender",
                       Test_File_Appender'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Appenders.List_Appender",
                       Test_List_Appender'Access);

      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Log (Perf)",
                       Test_Log_Perf'Access);
   end Add_Tests;

end Util.Log.Tests;
