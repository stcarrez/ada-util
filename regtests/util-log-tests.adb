-----------------------------------------------------------------------
--  log.tests -- Unit tests for loggers
--  Copyright (C) 2009, 2010, 2011, 2013, 2015, 2018, 2021, 2022 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Util.Test_Caller;

with Util.Log.Loggers;
with Util.Files;
with Util.Strings;
with Util.Properties;
with Util.Measures;
package body Util.Log.Tests is

   Log : constant Loggers.Logger := Loggers.Create ("util.log.test");

   procedure Test_Log (T : in out Test) is
      L : Loggers.Logger := Loggers.Create ("util.log.test.debug");
   begin
      L.Set_Level (DEBUG_LEVEL);
      Log.Info ("My log message");
      Log.Error ("My error message");
      Log.Debug ("A debug message Not printed");

      L.Info ("An info message");
      L.Info ("A {0} {1} {2} {3}", "info", "message", "not", "printed");
      L.Debug ("A debug message on logger 'L'");
      Util.Tests.Assert_Equals (T, "DEBUG", L.Get_Level_Name,
                               "Get_Level_Name function is invalid");
   end Test_Log;

   procedure Test_Debug (T : in out Test) is
      L : Loggers.Logger := Loggers.Create ("util.log.test.debug");
      C : Ada.Strings.Unbounded.Unbounded_String;
   begin
      L.Set_Level (DEBUG_LEVEL);
      L.Info ("My log message");
      L.Error ("My error message");
      L.Debug ("A {0} {1} {2} {3}", "debug", "message", "not", "printed");
      L.Debug ("A {0} {1} {2} {3}", C, "message", "printed");

      L.Info ("An info message");
      L.Debug ("A debug message on logger 'L'");
      Util.Tests.Assert_Equals (T, "DEBUG", L.Get_Level_Name,
                               "Get_Level_Name function is invalid");
   end Test_Debug;

   --  ------------------------------
   --  Test custom levels.
   --  ------------------------------
   procedure Test_Level_Custom (T : in out Test) is
      L       : Loggers.Logger := Loggers.Create ("util.log.test.custom");
      Path    : constant String := Util.Tests.Get_Test_Path ("test-custom.log");
      Props   : Util.Properties.Manager;
   begin
      Props.Set ("log4j.appender.test", "File");
      Props.Set ("log4j.appender.test.File", Path);
      Props.Set ("log4j.appender.test.append", "false");
      Props.Set ("log4j.appender.test.immediateFlush", "true");
      Props.Set ("log4j.appender.test.layout", "message");
      Props.Set ("log4j.rootCategory", "DEBUG,test");
      Util.Log.Loggers.Initialize (Props);

      L.Set_Level (DEBUG_LEVEL);
      L.Info ("My log message");
      L.Print (Util.Log.FATAL_LEVEL, "Test fatal message (ignore)");
      L.Print (9, "Test custom level 9 message");

      Util.Tests.Assert_Equals (T, "FATAL", Get_Level_Name (FATAL_LEVEL),
                                "Invalid fatal level");
      Util.Tests.Assert_Equals (T, "ERROR", Get_Level_Name (ERROR_LEVEL),
                                "Invalid fatal level");
      Util.Tests.Assert_Equals (T, " 1", Get_Level_Name (1),
                                "Invalid error level");
      Util.Tests.Assert_Equals (T, FATAL_LEVEL, Get_Level ("FATAL"),
                                "Invalid error level");
      Util.Tests.Assert_Equals (T, ERROR_LEVEL, Get_Level ("ERROR"),
                                "Invalid error level");

   end Test_Level_Custom;

   --  ------------------------------
   --  Test configuration and creation of file
   --  ------------------------------
   procedure Test_File_Appender (T : in out Test) is
      Path  : constant String := Util.Tests.Get_Test_Path ("test.log");
      Props : Util.Properties.Manager;
   begin
      Props.Set ("log4j.appender.test", "File");
      Props.Set ("log4j.appender.test.File", Path);
      Props.Set ("log4j.appender.test.layout", "level-message");
      Props.Set ("log4j.logger.util.log.test.file", "DEBUG,test");
      Util.Log.Loggers.Initialize (Props);

      declare
         L : constant Loggers.Logger := Loggers.Create ("util.log.test.file");
      begin
         L.Debug ("Writing a debug message");
         L.Debug ("{0}: {1}", "Parameter", "Value");
      end;
      T.Assert (Ada.Directories.Exists (Path), "Log file " & Path & " not found");
   end Test_File_Appender;

   procedure Test_Log_Perf (T : in out Test) is
      Path  : constant String := Util.Tests.Get_Test_Path ("test_perf.log");
      Props : Util.Properties.Manager;
   begin
      Props.Set ("log4j.appender.test", "File");
      Props.Set ("log4j.appender.test.File", Path);
      Props.Set ("log4j.appender.test.layout", "full");
      Props.Set ("log4j.logger.util.log.test.perf", "DEBUG,test");
      Util.Log.Loggers.Initialize (Props);

      for I in 1 .. 1000 loop
         declare
            S : Util.Measures.Stamp;
         begin
            Util.Measures.Report (S, "Util.Measures.Report", 1000);
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
         Util.Measures.Report (S, "Log.Info message (output)", 1000);

         L.Set_Level (INFO_LEVEL);
         for I in 1 .. 10_000 loop
            L.Debug ("My log message: {0}: {1}", "A message",
                     "A second parameter");
         end loop;
         Util.Measures.Report (S, "Log.Debug message (no output)", 10_000);
      end;
      T.Assert (Ada.Directories.Exists (Path), "Log file " & Path & " not found");
   end Test_Log_Perf;

   --  ------------------------------
   --  Test appending the log on several log files
   --  ------------------------------
   procedure Test_List_Appender (T : in out Test) is
      use Ada.Strings;
      use Ada.Directories;
      Props : Util.Properties.Manager;
   begin
      for I in 1 .. 10 loop
         declare
            Id   : constant String := Fixed.Trim (Integer'Image (I), Both);
            Path : constant String := Util.Tests.Get_Test_Path ("test" & Id & ".log");
            Name : constant String := "log4j.appender.test" & Id;
         begin
            Props.Set (Name, "File");
            Props.Set (Name & ".File", Path);
            Props.Set (Name & ".layout", "date-level-message");
            if I > 5 then
               Props.Set (Name & ".level", "INFO");
            end if;
         end;
      end loop;
      Props.Set ("log4j.rootCategory", "DEBUG, test.log");
      Props.Set ("log4j.appender.test.log.File",
                 Util.Tests.Get_Test_Path ("test-default.log"));
      Props.Set ("log4j.logger.util.log.test.file",
                 "DEBUG,test4,test1 , test2,test3, test4, test5 ,  test6 , test7,test8,");
      Util.Log.Loggers.Initialize (Props);

      declare
         L : constant Loggers.Logger := Loggers.Create ("util.log.test.file");
      begin
         L.Debug ("Writing a debug message");
         L.Debug ("{0}: {1}", "Parameter", "Value");
         L.Debug ("Done");
      end;

      --  Check that we have non empty log files (up to test8.log).
      for I in 1 .. 8 loop
         declare
            Id   : constant String := Fixed.Trim (Integer'Image (I), Both);
            Path : constant String := Util.Tests.Get_Test_Path ("test" & Id & ".log");
         begin
            T.Assert (Ada.Directories.Exists (Path), "Log file " & Path & " not found");
            if I > 5 then
               T.Assert (Ada.Directories.Size (Path) < 100, "Log file "
                         & Path & " should be empty");
            else
               T.Assert (Ada.Directories.Size (Path) > 100, "Log file " & Path & " is empty");
            end if;
         end;
      end loop;
   end Test_List_Appender;

   --  ------------------------------
   --  Test file appender with different modes.
   --  ------------------------------
   procedure Test_File_Appender_Modes (T : in out Test) is
      use Ada.Directories;

      Append_Path  : constant String := Util.Tests.Get_Test_Path ("test-append.log");
      Append2_Path : constant String := Util.Tests.Get_Test_Path ("test-append2.log");
      Global_Path  : constant String := Util.Tests.Get_Test_Path ("test-append-global.log");
      Props        : Util.Properties.Manager;
   begin
      Props.Set ("log4j.appender.test", "File");
      Props.Set ("log4j.appender.test.File", Append_Path);
      Props.Set ("log4j.appender.test.append", "true");
      Props.Set ("log4j.appender.test.immediateFlush", "true");
      Props.Set ("log4j.appender.test_global", "File");
      Props.Set ("log4j.appender.test_global.File", Global_Path);
      Props.Set ("log4j.appender.test_global.append", "false");
      Props.Set ("log4j.appender.test_global.immediateFlush", "false");
      Props.Set ("log4j.logger.util.log.test.file", "DEBUG");
      Props.Set ("log4j.rootCategory", "DEBUG,test_global,test");
      Util.Log.Loggers.Initialize (Props);

      declare
         L : constant Loggers.Logger := Loggers.Create ("util.log.test.file");
      begin
         L.Debug ("Writing a debug message");
         L.Debug ("{0}: {1}", "Parameter", "Value");
         L.Debug ("Done");
         L.Error ("This is the error test message");
      end;

      Props.Set ("log4j.appender.test_append", "File");
      Props.Set ("log4j.appender.test_append.File", Append2_Path);
      Props.Set ("log4j.appender.test_append.append", "true");
      Props.Set ("log4j.appender.test_append.immediateFlush", "true");
      Props.Set ("log4j.logger.util.log.test2.file", "DEBUG,test_append,test_global");
      Util.Log.Loggers.Initialize (Props);

      declare
         L1 : constant Loggers.Logger := Loggers.Create ("util.log.test.file");
         L2 : constant Loggers.Logger := Loggers.Create ("util.log.test2.file");
      begin
         L1.Info ("L1-1 Writing a info message");
         L2.Info ("L2-2 {0}: {1}", "Parameter", "Value");
         L1.Info ("L1-3 Done");
         L2.Error ("L2-4 This is the error test2 message");
      end;

      Props.Set ("log4j.appender.test_append.append", "plop");
      Props.Set ("log4j.appender.test_append.immediateFlush", "falsex");
      Props.Set ("log4j.rootCategory", "DEBUG, test.log");
      Util.Log.Loggers.Initialize (Props);

      T.Assert (Ada.Directories.Size (Append_Path) > 100,
                "Log file test-append.log is empty");
      T.Assert (Ada.Directories.Size (Append2_Path) > 100,
                "Log file test-append2.log is empty");
      T.Assert (Ada.Directories.Size (Global_Path) > 100,
                "Log file test-append.log is empty");
   end Test_File_Appender_Modes;

   --  ------------------------------
   --  Test file appender with different modes.
   --  ------------------------------
   procedure Test_Console_Appender (T : in out Test) is
      Path    : constant String := Util.Tests.Get_Test_Path ("test_err.log");
      Props   : Util.Properties.Manager;
      File    : Ada.Text_IO.File_Type;
      Content : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
      Ada.Text_IO.Set_Error (File);

      Props.Set ("log4j.appender.test_console", "Console");
      Props.Set ("log4j.appender.test_console.stderr", "true");
      Props.Set ("log4j.appender.test_console.level", "WARN");
      Props.Set ("log4j.rootCategory", "INFO,test_console");
      Util.Log.Loggers.Initialize (Props);

      declare
         L : constant Loggers.Logger := Loggers.Create ("util.log.test.file");
      begin
         L.Debug ("Writing a debug message");
         L.Debug ("{0}: {1}", "Parameter", "Value");
         L.Debug ("Done");
         L.Info ("INFO MESSAGE!");
         L.Warn ("WARN MESSAGE!");
         L.Error ("This {0} {1} {2} test message", "is", "the", "error");
      end;
      Ada.Text_IO.Flush (Ada.Text_IO.Current_Error);
      Ada.Text_IO.Set_Error (Ada.Text_IO.Standard_Error);
      Ada.Text_IO.Close (File);

      Util.Files.Read_File (Path, Content);
      Util.Tests.Assert_Matches (T, ".*WARN MESSAGE!", Content,
                                 "Invalid console log (WARN)");
      Util.Tests.Assert_Matches (T, ".*This is the error test message", Content,
                                 "Invalid console log (ERROR)");
   exception
      when others =>
         Ada.Text_IO.Set_Error (Ada.Text_IO.Standard_Error);
         raise;

   end Test_Console_Appender;

   procedure Test_Missing_Config (T : in out Test) is
      pragma Unreferenced (T);
      L : constant Loggers.Logger := Loggers.Create ("util.log.test.debug");
   begin
      Util.Log.Loggers.Initialize ("plop");

      L.Info ("An info message");
      L.Debug ("A debug message on logger 'L'");
   end Test_Missing_Config;

   procedure Test_Log_Traceback (T : in out Test) is
      Path    : constant String := Util.Tests.Get_Test_Path ("test-traceback.log");
      Props   : Util.Properties.Manager;
      Content : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Props.Set ("log4j.appender.test", "File");
      Props.Set ("log4j.appender.test.File", Path);
      Props.Set ("log4j.appender.test.append", "false");
      Props.Set ("log4j.appender.test.immediateFlush", "true");
      Props.Set ("log4j.rootCategory", "DEBUG,test");
      Util.Log.Loggers.Initialize (Props);

      declare
         L : constant Loggers.Logger := Loggers.Create ("util.log.test.file");
      begin
         L.Error ("This is the error test message");
         raise Constraint_Error with "Test";

      exception
         when E : others =>
            L.Error ("Something wrong", E, True);
      end;

      Props.Set ("log4j.rootCategory", "DEBUG,console");
      Props.Set ("log4j.appender.console", "Console");
      Util.Log.Loggers.Initialize (Props);

      Util.Files.Read_File (Path, Content);
      Util.Tests.Assert_Matches (T, ".*Something wrong: Exception CONSTRAINT_ERROR:", Content,
                                 "Invalid console log (ERROR)");
   end Test_Log_Traceback;

   --  ------------------------------
   --  Test the rolling file appender.
   --  ------------------------------
   procedure Test_Rolling_File_Appender (T : in out Test) is
      Path    : constant String := Util.Tests.Get_Test_Path ("test_rolling.log");
      Dir     : constant String := Util.Tests.Get_Test_Path ("logs");
      Pattern : constant String := Util.Tests.Get_Test_Path ("logs/tst-roll-%i.log");
      Props   : Util.Properties.Manager;
   begin
      if Ada.Directories.Exists (Dir) then
         Ada.Directories.Delete_Tree (Dir);
      end if;

      Props.Set ("log4j.appender.test_rolling", "RollingFile");
      Props.Set ("log4j.appender.test_rolling.fileName", Path);
      Props.Set ("log4j.appender.test_rolling.filePattern", Pattern);
      Props.Set ("log4j.appender.test_rolling.policy", "size");
      Props.Set ("log4j.appender.test_rolling.minSize", "1000");
      Props.Set ("log4j.appender.test_rolling.strategy", "ascending");
      Props.Set ("log4j.appender.test_rolling.policyMin", "1");
      Props.Set ("log4j.appender.test_rolling.policyMax", "7");
      Props.Set ("log4j.appender.test_rolling.level", "DEBUG");
      Props.Set ("log4j.rootCategory", "DEBUG,test_rolling");
      Util.Log.Loggers.Initialize (Props);

      for I in 1 .. 1_000 loop
         declare
            L : constant Loggers.Logger := Loggers.Create ("util.log.test.file");
         begin
            L.Debug ("Writing a debug message");
            L.Debug ("{0}: {1}", "Parameter", "Value");
            L.Debug ("Done");
            L.Info ("INFO MESSAGE!");
            L.Warn ("WARN MESSAGE!");
            L.Error ("This {0} {1} {2} test message", "is", "the", "error");
         end;
      end loop;

      for I in 1 .. 7 loop
         declare
            use Util.Files;
            File : constant String
              := Compose (Dir, "tst-roll-" & Util.Strings.Image (I) & ".log");
         begin
            T.Assert (Ada.Directories.Exists (File), "Missing rolling file " & File);
         end;
      end loop;
   end Test_Rolling_File_Appender;

   package Caller is new Util.Test_Caller (Test, "Log");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Info",
                       Test_Log'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Debug",
                       Test_Debug'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Set_Level",
                       Test_Log'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Error",
                       Test_Log_Traceback'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Initialize",
                       Test_Missing_Config'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Appenders.File_Appender",
                       Test_File_Appender'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Appenders.File_Appender (append)",
                       Test_File_Appender_Modes'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Appenders.List_Appender",
                       Test_List_Appender'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Appenders.Console",
                       Test_Console_Appender'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Get_Level",
                       Test_Level_Custom'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Loggers.Log (Perf)",
                       Test_Log_Perf'Access);
      Caller.Add_Test (Suite, "Test Util.Log.Appenders.Rolling_Appender",
                       Test_Rolling_File_Appender'Access);
   end Add_Tests;

end Util.Log.Tests;
