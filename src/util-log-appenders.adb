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
with Ada.Calendar.Formatting;

package body Util.Log.Appenders is

   use Ada;

   --  ------------------------------
   --  Get the log level that triggers display of the log events
   --  ------------------------------
   function Get_Level (Self : in Appender) return Level_Type is
   begin
      return Self.Level;
   end Get_Level;

   --  ------------------------------
   --  Set the log level.
   --  ------------------------------
   procedure Set_Level (Self  : in out Appender;
                        Level : in Level_Type) is
   begin
      Self.Level := Level;
   end Set_Level;

   --  ------------------------------
   --  Format the event into a string
   --  ------------------------------
   function Format (Self  : in Appender;
                    Event : in Log_Event) return String is
      pragma Unreferenced (Self);
   begin
      return "[" & Calendar.Formatting.Image (Event.Time) & "] "
        & Get_Level_Name (Event.Level) & " - "
        & To_String (Event.Logger) & " - "
        & To_String (Event.Message);
   end Format;

   procedure Append (Self  : in out File_Appender;
                     Event : in Log_Event) is
   begin
      Text_IO.Put_Line (Self.Output, Format (Self, Event));
   end Append;

   --  ------------------------------
   --  Flush the log events.
   --  ------------------------------
   overriding
   procedure Flush (Self   : in out File_Appender) is
   begin
      Text_IO.Flush (Self.Output);
   end Flush;

   --  ------------------------------
   --  Create a file appender and configure it according to the properties
   --  ------------------------------
   function Create_File_Appender (Name       : in String;
                                  Properties : in Util.Properties.Manager)
                                  return Appender_Access is
      Path : constant String := Properties.Get (Name & ".File");
      Result : constant File_Appender_Access := new File_Appender;
   begin
      Text_IO.Open (File => Result.Output,
                    Name => Path,
                    Mode => Text_IO.Out_File);
      return Result.all'Access;
   exception
      when Text_IO.Name_Error =>
         Text_IO.Create (File => Result.Output, Name => Path);
         return Result.all'Access;
   end Create_File_Appender;

   --  ------------------------------
   --  Set the file where the appender will write the logs
   --  ------------------------------
   procedure Set_File (Self : in out File_Appender;
                       Path : in String) is
   begin
      Text_IO.Open (File => Self.Output,
                    Name => Path,
                    Mode => Text_IO.Out_File);
   end Set_File;

   procedure Append (Self  : in out Console_Appender;
                     Event : in Log_Event) is
   begin
      Text_IO.Put_Line (Format (Self, Event));
   end Append;

   --  ------------------------------
   --  Flush the log events.
   --  ------------------------------
   overriding
   procedure Flush (Self   : in out Console_Appender) is
   begin
      Text_IO.Flush;
   end Flush;

end Util.Log.Appenders;
