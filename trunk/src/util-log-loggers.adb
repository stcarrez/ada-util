-----------------------------------------------------------------------
--  Logs -- Utility Log Package
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
with Ada.Strings.Fixed;
with Ada.Containers;
with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;

package body Util.Log.Loggers is

   use Util;
   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Log.Appenders;

   package Appender_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Log.Appenders.Appender_Access, Ada.Strings.Hash, "=");

   --  The log manager controls the configuration of loggers.
   --  The log appenders are shared by loggers and they are created by
   --  the log manager when a logger is created.
   --
   protected type Log_Manager is

      --  Initialize the log environment with the property file.
      procedure Initialize (Name : in String);

      --  Initialize the log environment with the property file.
      procedure Initialize (Properties : in Util.Properties.Manager);

      --  Get the logger property
      function Get_Logger_Property (Name : in String) return String;

      --  Find the appender to be used for the given logger.
      --  Create the appender if necessary.
      entry Find_Appender (Name     : in String;
                           Appender : out Appender_Access);

   private
      Config           : Properties.Manager;
      Appenders        : Appender_Maps.Map;
      Default_Appender : Log.Appenders.Appender_Access;
   end Log_Manager;

   Manager : Log_Manager;

   function Get_Appender (Value : in String) return String;

   function Get_Level (Value : in String) return Level_Type;

   function Find_Level (Name : in String) return Level_Type;

   function Format (Message : in String;
                    Arg1    : in String;
                    Arg2    : in String;
                    Arg3    : in String) return Unbounded_String;

   --  ------------------------------
   --  Initialize the log environment with the property file.
   --  ------------------------------
   procedure Initialize (Name : in String) is
   begin
      Manager.Initialize (Name);
   end Initialize;

   --  ------------------------------
   --  Initialize the log environment with the properties.
   --  ------------------------------
   procedure Initialize (Properties : in Util.Properties.Manager) is
   begin
      Manager.Initialize (Properties);
   end Initialize;

   protected body Log_Manager is

      --  ------------------------------
      --  Initialize the log environment with the property file.
      --  ------------------------------
      procedure Initialize (Name : in String) is
         F : File_Type;
      begin
         Manager.Default_Appender := new Log.Appenders.Console_Appender;
         Open (F, In_File, Name);
         Properties.Load_Properties (Config, F);
         Close (F);
      end Initialize;

      --  ------------------------------
      --  Initialize the log environment with the properties.
      --  ------------------------------
      procedure Initialize (Properties : in Util.Properties.Manager) is
      begin
         Config.Copy (From => Properties, Prefix => "log4j.");
      end Initialize;

      --  ------------------------------
      --  Get the logger property associated with a given logger
      --  ------------------------------
      function Get_Logger_Property (Name : in String) return String is
         Prop_Name : constant String := "log4j.logger." & Name;
      begin
         return Trim (Properties.Get (Config, Prop_Name), Both);

      exception
         when Properties.NO_PROPERTY =>
            declare
               Pos : constant Natural := Index (Name, ".",
                                                Ada.Strings.Backward);
            begin
               if Pos <= Name'First then
                  return "INFO";
               else
                  return Get_Logger_Property (Name (Name'First .. Pos - 1));
               end if;
            end;
      end Get_Logger_Property;

      --  ------------------------------
      --  Find an appender given its name
      --  ------------------------------
      entry Find_Appender (Name     : in String;
                           Appender : out Appender_Access) when True is
         use Appender_Maps;

         Value          : constant String := Get_Logger_Property (Name);
         Appender_Name  : constant String := Get_Appender (Value);
         Pos : constant Appender_Maps.Cursor
           := Manager.Appenders.Find (Appender_Name);
      begin
         if Pos /= Appender_Maps.No_Element then
            Appender := Appender_Maps.Element (Pos);
            return;
         end if;

         declare
            Appender_Type : constant String
              := Config.Get ("log4j.appender." & Appender_Name, "console");
         begin
            if Index (Appender_Type, "File") > 0 then
               Appender := Create_File_Appender ("log4j.appender."
                                                 & Appender_Name, Config);

               Manager.Appenders.Insert (Appender_Name, Appender);
            else
               if Manager.Default_Appender = null then
                  Manager.Default_Appender := new Console_Appender;
               end if;
               Appender := Manager.Default_Appender;
            end if;
         end;
      end Find_Appender;

   end Log_Manager;

   --  ------------------------------
   --  Get the log level from the property value
   --  ------------------------------
   function Get_Level (Value : in String) return Level_Type is
      Val : constant String  := Trim (Value, Both);
      Pos : constant Natural := Index (Val, ",");
   begin
      if Pos > Val'First then
         return Get_Level (Val (Val'First .. Pos - 1));
      elsif Val = "INFO" then
         return INFO_LEVEL;
      elsif Val = "DEBUG" then
         return DEBUG_LEVEL;
      elsif Val = "WARN" then
         return WARN_LEVEL;
      elsif Val = "ERROR" then
         return ERROR_LEVEL;
      elsif Val = "FATAL" then
         return FATAL_LEVEL;
      else
         return INFO_LEVEL;
      end if;
   end Get_Level;

   --  ------------------------------
   --  Get the log appender name from the property value
   --  ------------------------------
   function Get_Appender (Value : in String) return String is
      Pos : constant Natural := Index (Value, ",");
   begin
      if Pos <= Value'First or Pos >= Value'Last then
         return "root";
      else
         return Trim (Value (Pos + 1 .. Value'Last), Both);
      end if;
   end Get_Appender;

   --  ------------------------------
   --  Find the log level associated with the logger
   --  ------------------------------
   function Find_Level (Name : in String) return Level_Type is
      Value : constant String  := Manager.Get_Logger_Property (Name);
   begin
      return Get_Level (Value);
   end Find_Level;

   --  ------------------------------
   --  Create a logger with the given name.
   --  ------------------------------
   function Create (Name : in String) return Logger is
      Appender : Appender_Access;
   begin
      Manager.Find_Appender (Name, Appender);
      return Logger '(Ada.Finalization.Limited_Controlled with
                      Level    => Find_Level (Name),
                      Name     => To_Unbounded_String (Name),
                      Appender => Appender);
   end Create;

   --  ------------------------------
   --  Change the log level
   --  ------------------------------
   procedure Set_Level (Log   : in out Logger;
                        Level : in Level_Type) is
   begin
      Log.Level := Level;
   end Set_Level;

   --  ------------------------------
   --  Get the log level.
   --  ------------------------------
   function Get_Level (Log : in Logger) return Level_Type is
   begin
      return Log.Level;
   end Get_Level;

   --  ------------------------------
   --  Format the message with the arguments
   --  ------------------------------
   function Format (Message : in String;
                    Arg1    : in String;
                    Arg2    : in String;
                    Arg3    : in String) return Unbounded_String is
      Result : Unbounded_String := To_Unbounded_String (Message);
      Pos    : Natural;
      C      : Character;
   begin
      --  Replace {N} with arg1, arg2, arg3 or ?
      loop
         Pos := Index (Result, "{");
         exit when Pos = 0 or Pos + 2 > Length (Result)
           or Element (Result, Pos + 2) /= '}';
         C := Element (Result, Pos + 1);
         case C is
            when '0' =>
               Replace_Slice (Result, Pos, Pos + 2, Arg1);

            when '1' =>
               Replace_Slice (Result, Pos, Pos + 2, Arg2);

            when '2' =>
               Replace_Slice (Result, Pos, Pos + 2, Arg3);

            when others =>
               Replace_Slice (Result, Pos, Pos + 2, "?");
         end case;
      end loop;
      return Result;
   end Format;

   procedure Print (Log     : in Logger;
                    Level   : in Level_Type;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "") is
   begin
      if Log.Level >= Level then
         declare
            Event : Util.Log.Appenders.Log_Event;
         begin
            Event.Time    := Ada.Calendar.Clock;
            Event.Level   := Level;
            Event.Message := Format (Message, Arg1, Arg2, Arg3);
            Event.Logger  := Log.Name;
            Log.Appender.Append (Event);
         end;
      end if;
   end Print;

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "") is
   begin
      Print (Log, DEBUG_LEVEL, Message, Arg1, Arg2, Arg3);
   end Debug;

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      Print (Log, INFO_LEVEL, Message, Arg1, Arg2, Arg3);
   end Info;

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in Unbounded_String;
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      if Log.Level >= INFO_LEVEL then
         Print (Log, INFO_LEVEL, Message, To_String (Arg1), Arg2, Arg3);
      end if;
   end Info;

   procedure Warn (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      if Log.Level >= WARN_LEVEL then
         Print (Log, WARN_LEVEL, Message, Arg1, Arg2, Arg3);
      end if;
   end Warn;

   procedure Error (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "") is
   begin
      Print (Log, ERROR_LEVEL, Message, Arg1, Arg2, Arg3);
   end Error;

   procedure Error (Log     : in Logger'Class;
                    Message : in String;
                    E       : in Exception_Occurrence) is
   begin
      Print (Log, ERROR_LEVEL,
             "{0}: Exception {1}: {2}",
             Message,
             Exception_Name (E),
             Exception_Message (E));
   end Error;

   --  ------------------------------
   --  Set the appender that will handle the log events
   --  ------------------------------
   procedure Set_Appender (Log      : in out Logger'Class;
                           Appender : in Util.Log.Appenders.Appender_Access) is
   begin
      Log.Appender := Appender;
   end Set_Appender;

   --  ------------------------------
   --  Finalize the logger and flush the associated appender
   --  ------------------------------
   procedure Finalize (Log : in out Logger) is
   begin
      if Log.Appender /= null then
         Log.Appender.Flush;
      end if;
   end Finalize;

end Util.Log.Loggers;
