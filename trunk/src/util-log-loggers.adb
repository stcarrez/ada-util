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
with Ada.Containers;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Unchecked_Deallocation;
with Ada.IO_Exceptions;
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

      procedure Delete_Appenders;

      Config           : Properties.Manager;
      Appenders        : Appender_Maps.Map;
      Default_Level    : Level_Type := INFO_LEVEL;
      Default_Appender : Log.Appenders.Appender_Access := null;
      First_Logger     : Logger_Info_Access := null;
   end Log_Manager;

   Manager : Log_Manager;

   function Get_Appender (Value : in String) return String;

   function Format (Message : in String;
                    Arg1    : in String;
                    Arg2    : in String;
                    Arg3    : in String) return Unbounded_String;


   --  ------------------------------
   --  Get the logger property associated with a given logger
   --  ------------------------------
   function Get_Logger_Property (Properties : in Util.Properties.Manager;
                                 Name : in String) return String is
      Prop_Name : constant String := "log4j.logger." & Name;
   begin
      return Trim (Properties.Get (Prop_Name), Both);

   exception
      when Util.Properties.NO_PROPERTY =>
         declare
            Pos : constant Natural := Index (Name, ".",
                                             Ada.Strings.Backward);
         begin
            if Pos <= Name'First then
               return "";
            else
               return Get_Logger_Property (Properties, Name (Name'First .. Pos - 1));
            end if;
         end;
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
         F : File_Type;
      begin
         Open (F, In_File, Name);
         Properties.Load_Properties (Config, F);
         Close (F);

         Initialize_Again;

      exception
         when Ada.IO_Exceptions.Name_Error =>
            declare
               Event : Util.Log.Appenders.Log_Event;
            begin
               Event.Time    := Ada.Calendar.Clock;
               Event.Level   := WARN_LEVEL;
               Event.Message := Format ("Log configuration file {0} not found", Name, "", "");
               Event.Logger  := Ada.Strings.Unbounded.To_Unbounded_String ("Init");
               Default_Appender.Append (Event);
            end;
      end Initialize;

      --  ------------------------------
      --  Re-initializes the loggers after the configuration is changed.
      --  ------------------------------
      procedure Initialize_Again is
         L : Logger_Info_Access := First_Logger;
      begin
         Delete_Appenders;

         --  Initialize the default category.
         if Config.Exists ("log4j.rootCategory") then
            declare
               Value : constant String := Config.Get ("log4j.rootCategory");
            begin
               Default_Level := Get_Level (Value, Default_Level);
               Find_Appender (Property => Value, Appender => Default_Appender);
            end;
         end if;
         if Default_Appender = null then
            Default_Appender := new Log.Appenders.Console_Appender;
         end if;
         Appenders.Insert (Key => "root", New_Item => Default_Appender);

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
      begin
         Config.Copy (From => Properties, Prefix => "log4j.");
         Initialize_Again;
      end Initialize;

      --  ------------------------------
      --  Initialize the logger by reading the configuration, setting its level
      --  and creating the appender
      --  ------------------------------
      procedure Initialize (Log : in out Logger_Info) is
         Name : constant String := To_String (Log.Name);
         Prop : constant String := Get_Logger_Property (Config, Name);
      begin
         Log.Level := Get_Level (Prop, Default_Level);
         Find_Appender (Prop, Log.Appender);
      end Initialize;

      --  ------------------------------
      --  Create and initialize the logger
      --  ------------------------------
      procedure Create (Name : in String;
                        Log  : out Logger_Info_Access) is
      begin
         Log       := new Logger_Info;
         Log.Name  := To_Unbounded_String (Name);
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

      procedure Delete_Appenders is
         procedure Free is new Ada.Unchecked_Deallocation (Object => Appender'Class,
                                                           Name   => Appender_Access);

         Iter : Appender_Maps.Cursor;
      begin
         loop
            Iter := Appenders.First;
            exit when not Appender_Maps.Has_Element (Iter);
            declare
               E : Appender_Access := Appender_Maps.Element (Iter);
            begin
               Appenders.Delete (Iter);
               Free (E);
            end;
         end loop;
         Default_Appender := null;
      end Delete_Appenders;

      --  ------------------------------
      --  Obtain an appender given its name.  If the appender does not exist, it is created.
      --  ------------------------------
      procedure Build_Appender (Name     : in String;
                                Appender : out Appender_Access) is
         use Appender_Maps;

         Pos : constant Appender_Maps.Cursor := Appenders.Find (Name);
      begin
         if Pos /= Appender_Maps.No_Element then
            Appender := Appender_Maps.Element (Pos);
            return;
         end if;
         declare
            Prop_Name     : constant String := "log4j.appender." & Name;
            Appender_Type : constant String := Config.Get (Prop_Name, "console");
         begin
            if Index (Appender_Type, "File") > 0 then
               Appender := Create_File_Appender (Prop_Name, Config, Default_Level);
            else
               Appender := Create_Console_Appender (Prop_Name, Config, Default_Level);
            end if;
            Appenders.Insert (Name, Appender);
         end;
      end Build_Appender;

      --  ------------------------------
      --  Find an appender given the property value
      --  ------------------------------
      procedure Find_Appender (Property : in String;
                               Appender : out Appender_Access) is
         use Appender_Maps;

         Appender_Name  : constant String := Get_Appender (Property);
         Pos            : constant Appender_Maps.Cursor := Appenders.Find (Appender_Name);
      begin
         if Pos /= Appender_Maps.No_Element then
            Appender := Appender_Maps.Element (Pos);
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
               List := Create_List_Appender;
               loop
                  Build_Appender (Trim (Appender_Name (Last_Pos .. N - 1), Both), A);
                  List.Add_Appender (A);
                  exit when N > Appender_Name'Last;
                  Last_Pos := N + 1;
                  N := Ada.Strings.Fixed.Index (Appender_Name, ",", Last_Pos);
                  if N = 0 then
                     N := Appender_Name'Last + 1;
                  end if;
               end loop;
               Appender := List.all'Access;
               Appenders.Insert (Appender_Name, Appender);
            else
               Build_Appender (Appender_Name, Appender);
            end if;
         end;
      end Find_Appender;

   end Log_Manager;

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
   --  Create a logger with the given name.
   --  ------------------------------
   function Create (Name : in String) return Logger is
      Log : Logger_Info_Access;
   begin
      Manager.Create (Name, Log);
--        declare
--           Event : Util.Log.Appenders.Log_Event;
--        begin
--           Event.Time    := Ada.Calendar.Clock;
--           Event.Level   := DEBUG_LEVEL;
--           Event.Message := Format ("Create logger {0}", Name, "", "");
--           Event.Logger  := To_Unbounded_String (Name);
--           Log.Appender.Append (Event);
--        end;
      return Logger '(Ada.Finalization.Limited_Controlled with
                      Name     => To_Unbounded_String (Name),
                      Instance => Log);
   end Create;

   --  ------------------------------
   --  Initialize the logger and create a logger with the given name.
   --  ------------------------------
   function Create (Name   : in String;
                    Config : in String) return Logger is
   begin
      Initialize (Config);
      return Create (Name);
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

   --  ------------------------------
   --  Format the message with the arguments
   --  ------------------------------
   function Format (Message : in String;
                    Arg1    : in String;
                    Arg2    : in String;
                    Arg3    : in String) return Unbounded_String is
      Result : Unbounded_String := To_Unbounded_String (Message);
      Pos    : Natural := 1;
      C      : Character;
   begin
      --  Replace {N} with arg1, arg2, arg3 or ?
      loop
         Pos := Index (Result, "{", Pos);
         exit when Pos = 0 or Pos + 2 > Length (Result)
           or Element (Result, Pos + 2) /= '}';
         C := Element (Result, Pos + 1);
         case C is
            when '0' =>
               Replace_Slice (Result, Pos, Pos + 2, Arg1);
               Pos := Pos + Arg1'Length;

            when '1' =>
               Replace_Slice (Result, Pos, Pos + 2, Arg2);
               Pos := Pos + Arg2'Length;

            when '2' =>
               Replace_Slice (Result, Pos, Pos + 2, Arg3);
               Pos := Pos + Arg3'Length;

            when others =>
               Replace_Slice (Result, Pos, Pos + 2, "?");
               Pos := Pos + 2;
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
      if Log.Instance.Level >= Level then
         declare
            Event : Util.Log.Appenders.Log_Event;
         begin
            Event.Time    := Ada.Calendar.Clock;
            Event.Level   := Level;
            Event.Message := Format (Message, Arg1, Arg2, Arg3);
            Event.Logger  := Log.Name;
            Log.Instance.Appender.Append (Event);
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
                    Arg1    : in Unbounded_String;
                    Arg2    : in String := "";
                    Arg3    : in String := "") is
   begin
      if Log.Instance.Level >= DEBUG_LEVEL then
         Print (Log, DEBUG_LEVEL, Message, To_String (Arg1), Arg2, Arg3);
      end if;
   end Debug;

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in Unbounded_String;
                    Arg2    : in Unbounded_String;
                    Arg3    : in String := "") is
   begin
      if Log.Instance.Level >= DEBUG_LEVEL then
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
                   Arg1    : in Unbounded_String;
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      if Log.Instance.Level >= INFO_LEVEL then
         Print (Log, INFO_LEVEL, Message, To_String (Arg1), Arg2, Arg3);
      end if;
   end Info;

   procedure Warn (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      if Log.Instance.Level >= WARN_LEVEL then
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
      Log.Instance.Appender := Appender;
   end Set_Appender;

   --  ------------------------------
   --  Finalize the logger and flush the associated appender
   --  ------------------------------
   procedure Finalize (Log : in out Logger) is
   begin
      Log.Debug ("Finalize logger {0}", Log.Name);
      if Log.Instance.Appender /= null then
         Log.Instance.Appender.Flush;
      end if;
      Manager.Remove (Log.Instance);
   end Finalize;

end Util.Log.Loggers;
