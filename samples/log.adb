-----------------------------------------------------------------------
--  log -- Log example
--  Copyright (C) 2010, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Log;
with Util.Log.Loggers;
with Syslog_Appenders;
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
   Syslog_Appenders.Syslog_Factory.Register;

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
