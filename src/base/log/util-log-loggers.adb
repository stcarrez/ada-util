-----------------------------------------------------------------------
--  util-log-loggers -- Utility Log Package
--  Copyright (C) 2001 - 2022 Stephane Carrez
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
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Ada.IO_Exceptions;
with Util.Strings;
with Util.Strings.Builders;
with Util.Strings.Formats;
with Util.Log.Appenders.Factories;
with Util.Log.Appenders.Consoles;
with Util.Log.Appenders.Files;
with Util.Log.Appenders.Rolling_Files;
package body Util.Log.Loggers is

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Log.Appenders;

   function Traceback (E : in Exception_Occurrence) return String is separate;

   package File_Factory is
      new Util.Log.Appenders.Factories (Name   => "File",
                                        Create => Files.Create'Access);

   package Rolling_File_Factory is
      new Util.Log.Appenders.Factories (Name   => "RollingFile",
                                        Create => Rolling_Files.Create'Access);

   package Console_Factory is
      new Util.Log.Appenders.Factories (Name   => "Console",
                                        Create => Consoles.Create'Access);

   --  The log manager controls the configuration of loggers.
   --  The log appenders are shared by loggers and they are created by
   --  the log manager when a logger is created.
   --
   protected type Log_Manager is

      --  Initialize the log environment with the property file.
      procedure Initialize (Name : in String);

      --  Initialize the log environment with the property file.
      procedure Initialize (Properties : in Util.Properties.Manager);

      --  Create and initialize the logger
      procedure Create (Name : in String;
                        Log  : out Logger_Info_Access);

      --  Remove the logger from the list
      procedure Remove (Log : in out Logger_Info_Access);

   private

      --  Initialize the logger by reading the configuration, setting its level
      --  and creating the appender
      procedure Initialize (Log : in out Logger_Info);

      --  Re-initializes the loggers after the configuration is changed.
      procedure Initialize_Again;

      --  Find the appender to be used for the given logger.
      --  Create the appender if necessary.
      procedure Find_Appender (Property : in String;
                               Appender : out Appender_Access);

      --  Obtain an appender given its name.  If the appender does not exist, it is created.
      procedure Build_Appender (Name     : in String;
                                Appender : out Appender_Access);

      procedure Create_Default_Appender;

      Config           : Properties.Manager;
      Appenders        : Log.Appenders.Appender_List;
      Default_Level    : Level_Type := WARN_LEVEL;
      Default_Appender : Log.Appenders.Appender_Access := null;
      First_Logger     : Logger_Info_Access := null;
   end Log_Manager;

   Manager : Log_Manager;

   function Get_Appender (Value : in String) return String with Inline_Always;

   --  Get the logger property associated with a given logger
   function Get_Logger_Property (Properties : in Util.Properties.Manager;
                                 Name       : in String) return String;

   --  ------------------------------
   --  Get the log appender name from the property value
   --  ------------------------------
   function Get_Appender (Value : in String) return String is
      Pos : constant Natural := Index (Value, ",");
   begin
      if Pos <= Value'First or else Pos >= Value'Last then
         return "";
      else
         return Trim (Value (Pos + 1 .. Value'Last), Both);
      end if;
   end Get_Appender;

   --  ------------------------------
   --  Get the logger property associated with a given logger
   --  ------------------------------
   function Get_Logger_Property (Properties : in Util.Properties.Manager;
                                 Name       : in String) return String is
      Prop_Name : constant String := "logger." & Name;
      Pos       : Natural := Prop_Name'Last;
   begin
      while Pos > Prop_Name'First loop
         if Properties.Exists (Prop_Name (Prop_Name'First .. Pos)) then
            return Trim (Properties.Get (Prop_Name (Prop_Name'First .. Pos)), Both);
         end if;
         Pos := Util.Strings.Rindex (Prop_Name, '.', Pos);
         if Pos > 0 then
            Pos := Pos - 1;
         end if;
      end loop;
      return "";
   end Get_Logger_Property;

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
      begin
         Util.Properties.Load_Properties (Config, Path => Name, Prefix => "log4j.", Strip => True);

         Initialize_Again;

      exception
         when Ada.IO_Exceptions.Name_Error =>
            declare
               Message : Util.Strings.Builders.Builder (256);
               Date    : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            begin
               Strings.Formats.Format (Message, "Log configuration file {0} not found", Name);
               if Default_Appender = null then
                  Create_Default_Appender;
               end if;
               Default_Appender.Append (Message, Date, WARN_LEVEL, "Util.Log");
            end;
      end Initialize;

      --  ------------------------------
      --  Re-initializes the loggers after the configuration is changed.
      --  ------------------------------
      procedure Initialize_Again is
         L : Logger_Info_Access := First_Logger;
      begin
         Util.Log.Appenders.Clear (Appenders);
         Default_Appender := null;

         --  Initialize the default category.
         if Config.Exists ("rootCategory") then
            declare
               Value : constant String := Config.Get ("rootCategory");
            begin
               Default_Level := Get_Level (Value, Default_Level);
               Find_Appender (Property => Value, Appender => Default_Appender);
            end;
         end if;
         if Default_Appender = null then
            Create_Default_Appender;
         end if;

         --  Re-initialize the existing loggers.  Note that there is no concurrency
         --  protection if a thread calls 'Initialize' while another thread is using
         --  an already initialized logger.
         while L /= null loop
            Initialize (L.all);
            L := L.Next_Logger;
         end loop;
      end Initialize_Again;

      --  ------------------------------
      --  Initialize the log environment with the properties.
      --  ------------------------------
      procedure Initialize (Properties : in Util.Properties.Manager) is
         New_Config : Util.Properties.Manager;
      begin
         New_Config.Copy (From => Properties, Prefix => "log4j.", Strip => True);
         Config := New_Config;
         Initialize_Again;
      end Initialize;

      --  ------------------------------
      --  Initialize the logger by reading the configuration, setting its level
      --  and creating the appender
      --  ------------------------------
      procedure Initialize (Log : in out Logger_Info) is
         Prop     : constant String := Get_Logger_Property (Config, Log.Name);
         Appender : Appender_Access;
      begin
         Log.Level := Get_Level (Prop, Default_Level);
         Find_Appender (Prop, Appender);
         if Appender /= null then
            Log.Appender := Appender.all'Access;
         end if;
      end Initialize;

      --  ------------------------------
      --  Create and initialize the logger
      --  ------------------------------
      procedure Create (Name : in String;
                        Log  : out Logger_Info_Access) is
      begin
         Log       := new Logger_Info (Len => Name'Length);
         Log.Name  := Name;
         Initialize (Log.all);

         Log.Next_Logger := First_Logger;
         Log.Prev_Logger := null;
         if First_Logger /= null then
            First_Logger.Prev_Logger := Log;
         end if;
         First_Logger := Log;
      end Create;

      --  ------------------------------
      --  Remove the logger from the list
      --  ------------------------------
      procedure Remove (Log : in out Logger_Info_Access) is
         procedure Free is new Ada.Unchecked_Deallocation (Object => Logger_Info,
                                                           Name   => Logger_Info_Access);
      begin
         --  Remove first logger
         if Log = First_Logger then
            First_Logger := First_Logger.Next_Logger;
            if First_Logger /= null then
               First_Logger.Prev_Logger := null;
            end if;

            --  Remove last logger
         elsif Log.Next_Logger = null then
            Log.Prev_Logger.Next_Logger := null;

         else
            Log.Next_Logger.Prev_Logger := Log.Prev_Logger;
            Log.Prev_Logger.Next_Logger := Log.Next_Logger;
         end if;
         Free (Log);
      end Remove;

      --  ------------------------------
      --  Obtain an appender given its name.  If the appender does not exist, it is created.
      --  ------------------------------
      procedure Build_Appender (Name     : in String;
                                Appender : out Appender_Access) is
      begin
         Appender := Util.Log.Appenders.Find_Appender (Appenders, Name);
         if Appender /= null then
            return;
         end if;

         if Name'Length > 0 then
            Appender := Util.Log.Appenders.Create (Name, Config, Default_Level);
            if Appender /= null then
               Util.Log.Appenders.Add_Appender (Appenders, Appender);
            end if;
         end if;
      end Build_Appender;

      procedure Create_Default_Appender is
      begin
         if Default_Appender = null then
            Default_Appender := Consoles.Create ("root", Config, ERROR_LEVEL);
            Set_Layout (Default_Appender.all, MESSAGE);
            Util.Log.Appenders.Add_Appender (Appenders, Default_Appender);
         end if;
      end Create_Default_Appender;

      --  ------------------------------
      --  Find an appender given the property value
      --  ------------------------------
      procedure Find_Appender (Property : in String;
                               Appender : out Appender_Access) is
         Appender_Name  : constant String := Get_Appender (Property);
      begin
         if Appender_Name'Length = 0 then
            Appender := Default_Appender;
            if Appender = null then
               Create_Default_Appender;
               Appender := Default_Appender;
            end if;
            return;
         end if;

         Appender := Util.Log.Appenders.Find_Appender (Appenders, Appender_Name);
         if Appender /= null then
            return;
         end if;

         declare
            N        : Natural := Index (Appender_Name, ",");
            Last_Pos : Natural := Appender_Name'First;
            List     : List_Appender_Access;
            A        : Appender_Access;
         begin
            --  The appender configuration refers to a list of appenders.
            --  Example:  DEBUG, out1, console
            if N > 0 then
               List := Create_List_Appender (Appender_Name);
               loop
                  Build_Appender (Trim (Appender_Name (Last_Pos .. N - 1), Both), A);
                  exit when A = null;
                  List.Add_Appender (A);
                  exit when N > Appender_Name'Last;
                  Last_Pos := N + 1;
                  N := Ada.Strings.Fixed.Index (Appender_Name, ",", Last_Pos);
                  if N = 0 then
                     N := Appender_Name'Last + 1;
                  end if;
               end loop;
               Appender := List.all'Access;
               Util.Log.Appenders.Add_Appender (Appenders, Appender);
            else
               Build_Appender (Appender_Name, Appender);
            end if;
         end;
      end Find_Appender;

   end Log_Manager;

   --  ------------------------------
   --  Create a logger with the given name.
   --  ------------------------------
   function Create (Name : in String) return Logger is
      Log : Logger_Info_Access;
   begin
      Manager.Create (Name, Log);
      return Logger '(Ada.Finalization.Limited_Controlled with
                      Instance => Log);
   end Create;

   --  ------------------------------
   --  Create a logger with the given name and use the specified level.
   --  ------------------------------
   function Create (Name  : in String;
                    Level : in Level_Type) return Logger is
      Log : Logger_Info_Access;
   begin
      Manager.Create (Name, Log);
      Log.Level := Level;
      return Logger '(Ada.Finalization.Limited_Controlled with
                      Instance => Log);
   end Create;

   --  ------------------------------
   --  Change the log level
   --  ------------------------------
   procedure Set_Level (Log   : in out Logger;
                        Level : in Level_Type) is
   begin
      Log.Instance.Level := Level;
   end Set_Level;

   --  ------------------------------
   --  Get the log level.
   --  ------------------------------
   function Get_Level (Log : in Logger) return Level_Type is
   begin
      return Log.Instance.Level;
   end Get_Level;

   --  ------------------------------
   --  Get the log level name.
   --  ------------------------------
   function Get_Level_Name (Log : in Logger) return String is
   begin
      return Get_Level_Name (Log.Instance.Level);
   end Get_Level_Name;

   procedure Print (Log     : in Logger;
                    Level   : in Level_Type;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "";
                    Arg4    : in String := "") is
      Instance : constant Logger_Info_Access := Log.Instance;
   begin
      if Instance /= null and then Instance.Level >= Level then
         declare
            Result : Util.Strings.Builders.Builder (256);
            Date   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         begin
            Strings.Formats.Format (Result, Message, Arg1, Arg2, Arg3, Arg4);
            Instance.Appender.Append (Result, Date, Level, Instance.Name);
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

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String;
                    Arg2    : in String;
                    Arg3    : in String;
                    Arg4    : in String) is
   begin
      Print (Log, DEBUG_LEVEL, Message, Arg1, Arg2, Arg3, Arg4);
   end Debug;

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in Unbounded_String;
                    Arg2    : in String := "";
                    Arg3    : in String := "") is
   begin
      if Log.Instance /= null and then Log.Instance.Level >= DEBUG_LEVEL then
         Print (Log, DEBUG_LEVEL, Message, To_String (Arg1), Arg2, Arg3);
      end if;
   end Debug;

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in Unbounded_String;
                    Arg2    : in Unbounded_String;
                    Arg3    : in String := "") is
   begin
      if Log.Instance /= null and then Log.Instance.Level >= DEBUG_LEVEL then
         Print (Log, DEBUG_LEVEL, Message, To_String (Arg1), To_String (Arg2), Arg3);
      end if;
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
                   Arg1    : in String;
                   Arg2    : in String;
                   Arg3    : in String;
                   Arg4    : in String) is
   begin
      Print (Log, INFO_LEVEL, Message, Arg1, Arg2, Arg3, Arg4);
   end Info;

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in Unbounded_String;
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      if Log.Instance /= null and then Log.Instance.Level >= INFO_LEVEL then
         Print (Log, INFO_LEVEL, Message, To_String (Arg1), Arg2, Arg3);
      end if;
   end Info;

   procedure Warn (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      if Log.Instance /= null and then Log.Instance.Level >= WARN_LEVEL then
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
                    E       : in Exception_Occurrence;
                    Trace   : in Boolean := False) is
   begin

      if Trace then
         Print (Log, ERROR_LEVEL,
                "{0}: Exception {1}: {2} at {3}",
                Message,
                Exception_Name (E),
                Exception_Message (E),
                Traceback (E));
      else
         Print (Log, ERROR_LEVEL,
                "{0}: Exception {1}: {2}",
                Message,
                Exception_Name (E),
                Exception_Message (E));
      end if;
   end Error;

   --  ------------------------------
   --  Finalize the logger and flush the associated appender
   --  ------------------------------
   overriding
   procedure Finalize (Log : in out Logger) is
   begin
      if Log.Instance.Appender /= null then
         Log.Instance.Appender.Flush;
      end if;
      Manager.Remove (Log.Instance);
   end Finalize;

begin
   Console_Factory.Register;
   File_Factory.Register;
   Rolling_File_Factory.Register;
end Util.Log.Loggers;
