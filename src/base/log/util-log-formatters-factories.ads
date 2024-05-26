-----------------------------------------------------------------------
--  util-log-formatters-factories -- Factory for log formatters
--  Copyright (C) 2024 Stephane Carrez
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

--  == Custom formatter ==
--  The formatter is responsible for preparing the message to be displayed
--  by log appenders.  It takes the message string and its arguments and builds
--  the message.  The same formatted message is given to each log appender.
--
--  Using a custom formatter can be useful to change the message before it is
--  formatter, filter messages to hide sensitive information and so on.
--  Implementing a custom formatter is made in three steps:
--
--  * first by extending the `Util.Log.Formatters.Formatter` tagged type and
--    overriding the `Format` procedure.  The procedure gets the log message passed
--    to the `Debug`, `Info`, `Warn` or `Error` procedure as well as every parameter
--    passed to customize the final message.  It must populate a `Builder`
--    object with the formatted message.
--  * second by writing a `Create` function that allocates an instance of
--    the formatter and customizes it with some configuration properties.
--  * third by instantiating the `Util.Log.Formatters.Factories` generic package.
--    It contains an elaboration body that registers automatically the factory.
--
--  For example, the two first steps could be implemented as follows (methods are
--  not shown):
--
--     type NLS_Formatter (Length : Positive) is
--        new Util.Log.Formatters.Formatter (Length) with null record;
--     function Create (Name       : in String;
--                      Properties : in Util.Properties.Manager;
--                      Default    : in Util.Log.Level_Type)
--                   return Util.Log.Formatters.Formatter_Access;
--
--  Then, the package is instantiated as follows:
--
--     package NLS_Factory is
--       new Util.Log.Appenders.Factories (Name   => "NLS",
--                                         Create => Create'Access)
--       with Unreferenced;
--
--  To use the new registered formatter, it is necessary to declare some minimal
--  configuration.  A `log4j.formatter.<name>` definition must be declared for each
--  named formatter where `<name>` is the logical name of the formatter.  The
--  property must indicate the factory name that must be used (example: `NLS`).
--  The named formatter can have custom properties and they are passed to the
--  `Create` procedure when it is created.  Such properties can be used to customize
--  the behavior of the formatter.
--
--     log4j.formatter.nlsFormatter=NLS
--     log4j.formatter.nslFormatter.prop1=value1
--     log4j.formatter.nlsFormatter.prop2=value2
--
--  Once the named formatter is declared, it can be selected for one or several
--  logger by appending the string `:<name>` after the log level.  For example:
--
--     log4j.logger.X.Y=WARN:nlsFormatter
--
--  With the above configuration, the `X.Y` and its descendant loggers will use
--  the formatter identified by `nlsFormatter`.  Note that it is also possible
--  to specify an appender for the configuration:
--
--     log4j.logger.X.Y=INFO:nlsFormatter,console
--
--  The above configuration will use the `nlsFormatter` formatter and the `console`
--  appender to write on the console.
generic
   Name   : String;
   Create : Factory_Access;
package Util.Log.Formatters.Factories is

   pragma Elaborate_Body;

end Util.Log.Formatters.Factories;
