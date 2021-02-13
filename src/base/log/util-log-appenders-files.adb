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
with Ada.Finalization;
with Util.Properties.Basic;
with Util.Log.Appenders.Formatter;
package body Util.Log.Appenders.Files is

   use Ada;
   use Ada.Finalization;

   overriding
   procedure Append (Self    : in out File_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String) is
      procedure Write_File (Data : in String) with Inline_Always;

      procedure Write_File (Data : in String) is
      begin
         Text_IO.Put (Self.Output, Data);
      end Write_File;

      procedure Write is new Formatter (Write_File);
   begin
      if Self.Level >= Level then
         Write (Self, Message, Date, Level, Logger);
         Text_IO.New_Line (Self.Output);
         if Self.Immediate_Flush then
            Self.Flush;
         end if;
      end if;
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
   --  Flush and close the file.
   --  ------------------------------
   overriding
   procedure Finalize (Self : in out File_Appender) is
   begin
      Self.Flush;
      Text_IO.Close (File => Self.Output);
   end Finalize;

   --  ------------------------------
   --  Create a file appender and configure it according to the properties
   --  ------------------------------
   function Create (Name       : in String;
                    Properties : in Util.Properties.Manager;
                    Default    : in Level_Type)
                   return Appender_Access is
      use Util.Properties.Basic;

      Base   : constant String := "appender." & Name;
      Path   : constant String := Properties.Get (Base & ".File", Name & ".log");
      Append : constant Boolean := Boolean_Property.Get (Properties, Base & ".append", True);
      Result : constant File_Appender_Access
        := new File_Appender '(Limited_Controlled with Length => Name'Length,
                               Name => Name,
                               others => <>);
   begin
      Result.Set_Level (Name, Properties, Default);
      Result.Set_Layout (Name, Properties, FULL);
      Result.Set_File (Path, Append);
      Result.Immediate_Flush := Boolean_Property.Get (Properties, Base & ".immediateFlush", True);
      return Result.all'Access;
   end Create;

   --  ------------------------------
   --  Set the file where the appender will write the logs.
   --  When <tt>Append</tt> is true, the log message are appended to the existing file.
   --  When set to false, the file is cleared before writing new messages.
   --  ------------------------------
   procedure Set_File (Self   : in out File_Appender;
                       Path   : in String;
                       Append : in Boolean := True) is
      Mode   : Text_IO.File_Mode;
   begin
      if Append then
         Mode := Text_IO.Append_File;
      else
         Mode := Text_IO.Out_File;
      end if;
      Text_IO.Open (File => Self.Output,
                    Name => Path,
                    Mode => Mode);
   exception
      when Text_IO.Name_Error =>
         Text_IO.Create (File => Self.Output, Name => Path);
   end Set_File;

end Util.Log.Appenders.Files;
