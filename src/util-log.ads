-----------------------------------------------------------------------
--  util-log -- Utility Log Package
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011, 2017, 2018 Stephane Carrez
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

--  = Logging =
--  The `Util.Log` package and children provide a simple logging framework inspired
--  from the Java Log4j library.  It is intended to provide a subset of logging features
--  available in other languages, be flexible, extensible, small and efficient.  Having
--  log messages in large applications is very helpful to understand, track and fix complex
--  issues, some of them being related to configuration issues or interaction with other
--  systems.  The overhead of calling a log operation is negligeable when the log is disabled
--  as it is in the order of 30ns and reasonable for a file appender has it is in the order
--  of 5us.
--
--  == Using the log framework ==
--  A bit of terminology:
--
--  * A *logger* is the abstraction that provides operations to emit a message.  The message
--    is composed of a text, optional formatting parameters, a log level and a timestamp.
--  * A *formatter* is the abstraction that takes the information about the log to format
--    the final message.
--  * An *appender* is the abstraction that writes the message either to a console, a file
--    or some other final mechanism.
--
--  === Logger Declaration ===
--  Similar to other logging framework such as Log4j and Log4cxx, it is necessary to have
--  and instance of a logger to write a log message.  The logger instance holds the configuration
--  for the log to enable, disable and control the format and the appender that will receive
--  the message.  The logger instance is associated with a name that is used for the
--  configuration.  A good practice is to declare a `Log` instance in the package body or
--  the package private part to make available the log instance to all the package operations.
--  The instance is created by using the `Create` function.  The name used for the configuration
--  is free but using the full package name is helpful to control precisely the logs.
--
--    with Util.Log.Loggers;
--    package body X.Y is
--      Log : constant Util.Log.Loggers := Util.Log.Loggers.Create ("X.Y");
--    end X.Y;
--
--  === Logger Messages ===
--  A log message is associated with a log level which is used by the logger instance to
--  decide to emit or drop the log message.  To keep the logging API simple and make it easily
--  usable in the application, several operations are provided to write a message with different
--  log level.
--
--  A log message is a string that contains optional formatting markers that follow more or
--  less the Java MessageFormat class.  A parameter is represented by a number enclosed by `{}`.
--  The first parameter is represented by `{0}`, the second by `{1}` and so on.
--
--  The example below shows several calls to emit a log message with different levels:
--
--     Log.Error ("Cannot open file {0}: {1}", Path, "File does not exist");
--     Log.Warn ("The file {0} is empty", Path);
--     Log.Info ("Opening file {0}", Path);
--     Log.Debug ("Reading line {0}", Line);
--
--  === Log Configuration ===
--  The log configuration uses property files close to the Apache Log4j and to the
--  Apache Log4cxx configuration files.
--  The configuration file contains several parts to configure the logging framework:
--
--  * First, the *appender* configuration indicates the appender that exists and can receive
--    a log message.
--  * Second, a root configuration allows to control the default behavior of the logging
--    framework.  The root configuration controls the default log level as well as the
--    appenders that can be used.
--  * Last, a logger configuration is defined to control the logging level more precisely
--    for each logger.
--
--  Here is a simple log configuration that creates a file appender where log messages are
--  written.  The file appender is given the name `result` and is configured to write the
--  messages in the file `my-log-file.log`.  The file appender will use the `level-message`
--  format for the layout of messages.  Last is the configuration of the `X.Y` logger
--  that will enable only messages starting from the `WARN` level.
--
--    log4j.rootCategory=DEBUG,result
--    log4j.appender.result=File
--    log4j.appender.result.File=my-log-file.log
--    log4j.appender.result.layout=level-message
--    log4j.logger.X.Y=WARN
--
--
package Util.Log is

   pragma Preelaborate;

   subtype Level_Type is Natural;

   FATAL_LEVEL : constant Level_Type := 0;
   ERROR_LEVEL : constant Level_Type := 5;
   WARN_LEVEL  : constant Level_Type := 7;
   INFO_LEVEL  : constant Level_Type := 10;
   DEBUG_LEVEL : constant Level_Type := 20;

   --  Get the log level name.
   function Get_Level_Name (Level : Level_Type) return String;

   --  Get the log level from the property value
   function Get_Level (Value   : in String;
                       Default : in Level_Type := INFO_LEVEL) return Level_Type;

   --  The <tt>Logging</tt> interface defines operations that can be implemented for a
   --  type to report errors or messages.
   type Logging is limited interface;

   procedure Error (Log     : in out Logging;
                    Message : in String) is abstract;

end Util.Log;
