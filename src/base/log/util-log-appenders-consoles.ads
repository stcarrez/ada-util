-----------------------------------------------------------------------
--  util-log-appenders-consoles -- Console log appenders
--  Copyright (C) 2001 - 2019, 2021, 2023 Stephane Carrez
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
--  | stderr         | When 'true' or '1', use the console standard error,                  |
--  |                | by default the appender uses the standard output.                    |
--  | utf8           | When 'true', use a direct write on the console and avoid using       |
--  |                | `Ada.Text_IO`.                                                       |
--
package Util.Log.Appenders.Consoles is

   type Console_Appender (Length : Positive) is new Appender with private;
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
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
                   return Appender_Access;
private

   type Console_Appender (Length : Positive) is new Appender (Length) with record
      Stderr : Boolean := False;
      Utf8   : Boolean := False;
      Prefix : Util.Properties.Value;
   end record;

end Util.Log.Appenders.Consoles;
