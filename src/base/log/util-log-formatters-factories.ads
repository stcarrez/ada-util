-----------------------------------------------------------------------
--  util-log-formatters-factories -- Factory for log formatters
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  == Custom formatter ==
--  The formatter is responsible for preparing the message to be displayed
--  by log appenders.  It takes the message string and its arguments and builds
--  the message.  The same formatted message is given to each log appender.
--  This step is handled by `Format_Message`.  Then, each log appender can use
--  the log appender to format the log event which is composed of the log level,
--  the date of the event, the logger name.  This step is handled by `Format_Event`.
--
--  Using a custom formatter can be useful to change the message before it is
--  formatted, translate messages, filter messages to hide sensitive information
--  and so on.  Implementing a custom formatter is made in three steps:
--
--  * first by extending the `Util.Log.Formatters.Formatter` tagged type and
--    overriding one of the `Format_XXX` procedures.  The procedure gets the
--    log message passed to the `Debug`, `Info`, `Warn` or `Error` procedure
--    as well as every parameter passed to customize the final message.
--    It must populate a `Builder` object with the formatted message.
--  * second by writing a `Create` function that allocates an instance of
--    the formatter and customizes it with some configuration properties.
--  * third by instantiating the `Util.Log.Formatters.Factories` generic package.
--    It contains an elaboration body that registers automatically the factory.
--
--  For example, a formatter that translates the message when it is printed
--  can be created.  The two first steps could be implemented as follows (method
--  bodies are not shown):
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
--     log4j.formatter.nlsFormatter.prop1=value1
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
--
--  A formatter can also be configured specifically for an appender by
--  using the following configuration:
--
--     log4j.appender.console.formatter=nlsFormatter
--
--  In that configuration, the `Format_Event` function of the configured formatter
--  will be called to format the log level, date and logger's name.
generic
   Name   : String;
   Create : Factory_Access;
package Util.Log.Formatters.Factories is

   pragma Elaborate_Body;

end Util.Log.Formatters.Factories;
