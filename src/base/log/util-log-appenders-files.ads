-----------------------------------------------------------------------
--  util-log-appenders-files -- File log appenders
--  Copyright (C) 2001 - 2021 Stephane Carrez
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
with Util.Properties;

--  === File appender ===
--  The `File` appender recognises the following configurations:
--
--  | Name           | Description                                                          |
--  | -------------- | --------------------------------------------------------------       |
--  | layout         | Defines the format of the message printed by the appender.           |
--  | level          | Defines the minimum level above which messages are printed.          |
--  | File           | The path used by the appender to create the output file.             |
--  | append         | When 'true' or '1', the file is opened in append mode otherwise      |
--  |                | it is truncated (the default is to truncate).                        |
--  | immediateFlush | When 'true' or '1', the file is flushed after each message log.      |
--  |                | Immediate flush is useful in some situations to have the log file    |
--  |                | updated immediately at the expense of slowing down the processing    |
--  |                | of logs.                                                             |
--
package Util.Log.Appenders.Files is

   --  ------------------------------
   --  File appender
   --  ------------------------------
   type File_Appender (Length : Positive) is new Appender with private;
   type File_Appender_Access is access all File_Appender'Class;

   overriding
   procedure Append (Self    : in out File_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String);

   --  Set the file where the appender will write the logs.
   --  When <tt>Append</tt> is true, the log message are appended to the existing file.
   --  When set to false, the file is cleared before writing new messages.
   procedure Set_File (Self   : in out File_Appender;
                       Path   : in String;
                       Append : in Boolean := True);

   --  Flush the log events.
   overriding
   procedure Flush (Self   : in out File_Appender);

   --  Flush and close the file.
   overriding
   procedure Finalize (Self : in out File_Appender);

   --  Create a file appender and configure it according to the properties
   function Create (Name       : in String;
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
     return Appender_Access;

private

   type File_Appender (Length : Positive) is new Appender (Length) with record
      Output          : Ada.Text_IO.File_Type;
      Immediate_Flush : Boolean := False;
   end record;

end Util.Log.Appenders.Files;
