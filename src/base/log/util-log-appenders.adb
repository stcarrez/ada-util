-----------------------------------------------------------------------
--  util-log-appenders -- Log appenders
--  Copyright (C) 2001 - 2024 Stephane Carrez
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
with Ada.Calendar.Time_Zones;
with Ada.Strings;
with Ada.Strings.Fixed;

with Util.Properties.Basic;
with Util.Strings.Transforms;
package body Util.Log.Appenders is

   use Ada;
   use Ada.Finalization;

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
   procedure Set_Level (Self       : in out Appender;
                        Name       : in String;
                        Properties : in Util.Properties.Manager;
                        Level      : in Level_Type) is
      Prop_Name : constant String := "appender." & Name & ".level";
   begin
      if Properties.Exists (Prop_Name) then
         Self.Level := Get_Level (Properties.Get (Prop_Name), Level);
      else
         Self.Level := Level;
      end if;
   end Set_Level;

   procedure Set_Level (Self       : in out Appender;
                        Level      : in Level_Type) is
   begin
      Self.Level := Level;
   end Set_Level;

   --  ------------------------------
   --  Set the log layout format.
   --  ------------------------------
   procedure Set_Layout (Self       : in out Appender;
                         Name       : in String;
                         Properties : in Util.Properties.Manager;
                         Layout     : in Layout_Type) is
      use Ada.Strings;
      use Util.Strings.Transforms;
      use Util.Properties.Basic;
      Layout_Name : constant String := "appender." & Name & ".layout";
      Utc_Name    : constant String := "appender." & Name & ".utc";
   begin
      if Properties.Exists (Layout_Name) then
         declare
            Value : constant String
              := To_Lower_Case (Fixed.Trim (Properties.Get (Layout_Name), Both));
         begin
            if Value = "message" then
               Self.Layout := MESSAGE;
            elsif Value = "level-message" then
               Self.Layout := LEVEL_MESSAGE;
            elsif Value = "date-level-message" or else Value = "level-date-message" then
               Self.Layout := DATE_LEVEL_MESSAGE;
            else
               Self.Layout := FULL;
            end if;
         end;
      else
         Self.Layout := Layout;
      end if;
      if Properties.Exists (Utc_Name) then
         Self.Use_UTC := Boolean_Property.Get (Properties, Utc_Name);
      end if;
   end Set_Layout;

   procedure Set_Layout (Self       : in out Appender;
                         Layout     : in Layout_Type) is
   begin
      Self.Layout := Layout;
   end Set_Layout;

   function Format (Self : in Appender;
                    Date : in Ada.Calendar.Time) return String is
   begin
      if Self.Use_UTC then
         return Calendar.Formatting.Image (Date);
      else
         return Calendar.Formatting.Image
           (Date, False,
            Ada.Calendar.Time_Zones.UTC_Time_Offset (Date));
      end if;
   end Format;

   --  ------------------------------
   --  Format the event into a string
   --  ------------------------------
   function Format (Self : in Appender'Class;
                    Date : in Ada.Calendar.Time;
                    Level : in Level_Type;
                    Logger : in String) return String is
   begin
      case Self.Layout is
         when MESSAGE =>
            return "";

         when LEVEL_MESSAGE =>
            return Get_Level_Name (Level) & ": ";

         when DATE_LEVEL_MESSAGE =>
            return "[" & Self.Format (Date) & "] "
              & Get_Level_Name (Level) & ": ";

         when FULL =>
            return "[" & Self.Format (Date)
              & "] " & Get_Level_Name (Level) & " - " & Logger & " - : ";

      end case;
   end Format;

   overriding
   procedure Append (Self    : in out List_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String) is
   begin
      for I in 1 .. Self.Count loop
         Self.Appenders (I).Append (Message, Date, Level, Logger);
      end loop;
   end Append;

   --  ------------------------------
   --  Flush the log events.
   --  ------------------------------
   overriding
   procedure Flush (Self   : in out List_Appender) is
   begin
      for I in 1 .. Self.Count loop
         Self.Appenders (I).Flush;
      end loop;
   end Flush;

   --  ------------------------------
   --  Add the appender to the list.
   --  ------------------------------
   procedure Add_Appender (Self   : in out List_Appender;
                           Object : in Appender_Access) is
   begin
      if Self.Count < Self.Appenders'Last then
         Self.Count := Self.Count + 1;
         Self.Appenders (Self.Count) := Object;
      end if;
   end Add_Appender;

   --  ------------------------------
   --  Create a list appender and configure it according to the properties
   --  ------------------------------
   function Create_List_Appender (Name : in String) return List_Appender_Access is
      Result : constant List_Appender_Access
        := new List_Appender '(Limited_Controlled with Length => Name'Length,
                               Name => Name,
                               others => <>);
   begin
      return Result;
   end Create_List_Appender;

   --  ------------------------------
   --  Create an appender instance with a factory with the given name.
   --  ------------------------------
   function Create (Name     : in String;
                    Config   : in Util.Properties.Manager;
                    Default  : in Level_Type) return Appender_Access is
      Prop_Name     : constant String := "appender." & Name;
      Appender_Type : constant String := Config.Get (Prop_Name, "console");
      Factory       : Appender_Factory_Access := Appender_Factories;
   begin
      while Factory /= null loop
         if Factory.Name = Appender_Type then
            return Factory.Factory (Name, Config, Default);
         end if;
         Factory := Factory.Next_Factory;
      end loop;

      Factory := Appender_Factories;
      if Factory /= null then
         return Factory.Factory (Name, Config, Default);
      end if;
      return null;
   end Create;

end Util.Log.Appenders;
