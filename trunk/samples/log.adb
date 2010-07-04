-----------------------------------------------------------------------
--  log -- Log example
--  Copyright (C) 2010 Stephane Carrez
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
with Util.Log;
with Util.Log.Loggers;

procedure Log is

   use Util.Log;

   Log  : constant Loggers.Logger := Loggers.Create ("log");

   Log2 : constant Loggers.Logger := Loggers.Create ("log.util");

   procedure Report;

   procedure Report is
   begin
      Log2.Debug ("Report is called (debug message must be visible)");
   end Report;

begin
   --  Initialization is optional.  Get the log configuration by reading the property
   --  file 'samples/log4j.properties'.  The 'log.util' logger will use a DEBUG level
   --  and write the message in 'result.log'.
   Util.Log.Loggers.Initialize ("samples/log4j.properties");

   Log.Info ("Starting the log example, level is {0}", Log.Get_Level_Name);

   --  Next log will do nothing
   Log.Debug ("A debug message (this should be hidden): {0}", "None");
   Log.Info ("Log2 has the level {0}", Log2.Get_Level_Name);
   Report;
   Log.Warn ("A warning message");
   Log.Error ("An error message");
   Log.Info ("Debug message written in 'result.log'");
end Log;
