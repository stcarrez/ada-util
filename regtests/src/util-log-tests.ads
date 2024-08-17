-----------------------------------------------------------------------
--  log.tests -- Unit tests for loggers
--  Copyright (C) 2009 - 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Log.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Log_Perf (T : in out Test);
   procedure Test_Log (T : in out Test);
   procedure Test_Debug (T : in out Test);
   procedure Test_File_Appender (T : in out Test);
   procedure Test_List_Appender (T : in out Test);
   procedure Test_Console_Appender (T : in out Test);
   procedure Test_Missing_Config (T : in out Test);
   procedure Test_Log_Traceback (T : in out Test);

   --  Test custom levels.
   procedure Test_Level_Custom (T : in out Test);

   --  Test file appender with different modes.
   procedure Test_File_Appender_Modes (T : in out Test);

   --  Test the rolling file appender.
   procedure Test_Rolling_File_Appender (T : in out Test);

   --  Test using a custom formatter.
   procedure Test_Custom_Formatter (T : in out Test);

end Util.Log.Tests;
