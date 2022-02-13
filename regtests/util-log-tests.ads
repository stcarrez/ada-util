-----------------------------------------------------------------------
--  log.tests -- Unit tests for loggers
--  Copyright (C) 2009, 2010, 2011, 2015, 2021, 2022 Stephane Carrez
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

end Util.Log.Tests;
