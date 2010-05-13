-----------------------------------------------------------------------
--  Appenders -- Log appenders
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Util.Properties;

--  The log <b>Appender</b> will handle the low level operations to write
--  the log content to a file, the console, a database.
package Util.Log.Appenders is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Log event
   --  ------------------------------
   --  The <b>Log_Event</b> represent a log message reported by one of the
   --  <b>log</b> operation (Debug, Info, Warn, Error).
   type Log_Event is record
      --  The log message (formatted)
      Message : Unbounded_String;

      --  The timestamp when the message was produced.
      Time    : Ada.Calendar.Time;

      --  The log level
      Level   : Level_Type;

      --  The logger
      Logger  : Unbounded_String;
   end record;

   --  ------------------------------
   --  Log appender
   --  ------------------------------
   type Appender is abstract tagged limited private;

   type Appender_Access is access all Appender'Class;

   --  Get the log level that triggers display of the log events
   function Get_Level (Self : in Appender) return Level_Type;

   --  Set the log level.
   procedure Set_Level (Self  : in out Appender;
                        Level : in Level_Type);

   --  Format the event into a string
   function Format (Self  : in Appender;
                    Event : in Log_Event) return String;

   --  Append a log event to the appender.  Depending on the log level
   --  defined on the appender, the event can be taken into account or
   --  ignored.
   procedure Append (Self  : in out Appender;
                     Event : in Log_Event) is abstract;

   --  Flush the log events.
   procedure Flush (Self   : in out Appender) is abstract;

   --  ------------------------------
   --  File appender
   --  ------------------------------
   --  Write log events to a file
   type File_Appender is new Appender with private;
   type File_Appender_Access is access all File_Appender'Class;

   overriding
   procedure Append (Self  : in out File_Appender;
                     Event : in Log_Event);

   --  Set the file where the appender will write the logs
   procedure Set_File (Self : in out File_Appender;
                       Path : in String);

   --  Flush the log events.
   overriding
   procedure Flush (Self   : in out File_Appender);

   --  Create a file appender and configure it according to the properties
   function Create_File_Appender (Name       : in String;
                                  Properties : in Util.Properties.Manager)
     return Appender_Access;

   --  ------------------------------
   --  Console appender
   --  ------------------------------
   --  Write log events to the console
   type Console_Appender is new Appender with private;

   overriding
   procedure Append (Self  : in out Console_Appender;
                     Event : in Log_Event);

   --  Flush the log events.
   overriding
   procedure Flush (Self   : in out Console_Appender);

private

   type Appender is abstract tagged limited record
      Level    : Level_Type := INFO_LEVEL;
   end record;

   type File_Appender is new Appender with record
      Output : Ada.Text_IO.File_Type;
   end record;

   type Console_Appender is new Appender with null record;

end Util.Log.Appenders;
