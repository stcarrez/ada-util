-----------------------------------------------------------------------
--  util-log-lzma-tests - Test for rolling file with LZMA compression
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Directories;

with Util.Strings;
with Util.Properties;
with Util.Test_Caller;

with Util.Log.Loggers;
with Util.Files;
with Util.Log.Appenders.Factories;
with Util.Log.Appenders.Rolling_Files.Lzma;
package body Util.Log.Lzma_Tests is

   --  Factory for the Lzma rolling file appender.
   package Lzma_Rolling_Factory is
     new Util.Log.Appenders.Factories
       (Name   => "LzmaRollingFileTest",
        Create => Util.Log.Appenders.Rolling_Files.Lzma.Create'Access) with Unreferenced;

   --  ------------------------------
   --  Test the rolling file appender.
   --  ------------------------------
   procedure Test_Rolling_File_Appender (T : in out Test) is
      Path    : constant String := Util.Tests.Get_Test_Path ("test_rolling_lzma.log");
      Dir     : constant String := Util.Tests.Get_Test_Path ("logs-lzma");
      Pattern : constant String := Util.Tests.Get_Test_Path ("logs-lzma/tst-roll-%i.log.xz");
      Props   : Util.Properties.Manager;
   begin
      if Ada.Directories.Exists (Dir) then
         Ada.Directories.Delete_Tree (Dir);
      end if;

      Props.Set ("log4j.appender.test_rollingz", "LzmaRollingFile");
      Props.Set ("log4j.appender.test_rollingz.fileName", Path);
      Props.Set ("log4j.appender.test_rollingz.filePattern", Pattern);
      Props.Set ("log4j.appender.test_rollingz.policy", "size");
      Props.Set ("log4j.appender.test_rollingz.minSize", "1000");
      Props.Set ("log4j.appender.test_rollingz.strategy", "ascending");
      Props.Set ("log4j.appender.test_rollingz.policyMin", "1");
      Props.Set ("log4j.appender.test_rollingz.policyMax", "7");
      Props.Set ("log4j.appender.test_rollingz.level", "DEBUG");
      Props.Set ("log4j.rootCategory", "DEBUG,test_rollingz");
      Util.Log.Loggers.Initialize (Props);

      for I in 1 .. 1_000 loop
         declare
            L : constant Loggers.Logger := Loggers.Create ("util.log.test.filez");
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
              := Compose (Dir, "tst-roll-" & Util.Strings.Image (I) & ".log.xz");
         begin
            T.Assert (Ada.Directories.Exists (File), "Missing rolling file " & File);
         end;
      end loop;
   end Test_Rolling_File_Appender;

   package Caller is new Util.Test_Caller (Test, "Log.Lzma");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Log.Appenders.Rolling_Appender (LZMA)",
                       Test_Rolling_File_Appender'Access);
   end Add_Tests;

end Util.Log.Lzma_Tests;
