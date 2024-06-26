-----------------------------------------------------------------------
--  util-log-appenders-consoles -- Console log appenders
--  Copyright (C) 2001 - 2019, 2021, 2023, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  === Console appender ===
--  The `Console` appender uses either `Ada.Text_IO` or a direct write on console
--  to write messages.  The default is to use `Ada.Text_IO` and the appender expects
--  standard Ada strings encoded in Latin-1 in the configuration.  When the appender
--  gets UTF-8 strings, it should be configured for a direct write on the console.
--  The console appender recognises the following configurations:
--
--  | Name           | Description                                                          |
--  | -------------- | --------------------------------------------------------------       |
--  | layout         | Defines the format of the message printed by the appender.           |
--  | level          | Defines the minimum level above which messages are printed.          |
--  | utc            | When 'true' or '1', print the date in UTC instead of local time      |
--  | stderr         | When 'true' or '1', use the console standard error,                  |
--  |                | by default the appender uses the standard output.                    |
--  | utf8           | When 'true', use a direct write on the console and avoid using       |
--  |                | `Ada.Text_IO`.                                                       |
--
--  Example of configuration:
--
--    log4j.appender.console=Console
--    log4j.appender.console.level=WARN
--    log4j.appender.console.layout=level-message
--    log4j.appender.console.utc=false
package Util.Log.Appenders.Consoles is

   type Console_Appender (Length    : Positive;
                          Formatter : Formatter_Access) is new Appender with private;
   type Console_Appender_Access is access all Console_Appender'Class;

   overriding
   procedure Append (Self    : in out Console_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String);

   --  Flush the log events.
   overriding
   procedure Flush (Self   : in out Console_Appender);

   --  Create a console appender and configure it according to the properties
   function Create (Name       : in String;
                    Formatter  : in Formatter_Access;
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
                   return Appender_Access;
private

   type Console_Appender (Length    : Positive;
                          Formatter : Formatter_Access) is new Appender (Length, Formatter) with
   record
      Stderr : Boolean := False;
      Utf8   : Boolean := False;
      Prefix : Util.Properties.Value;
   end record;

end Util.Log.Appenders.Consoles;
