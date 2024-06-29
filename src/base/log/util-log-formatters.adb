-----------------------------------------------------------------------
--  util-log-formatters -- Log formatter
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;
package body Util.Log.Formatters is

   procedure Format_Message (Format    : in Formatter;
                             Into      : in out Util.Strings.Builders.Builder;
                             Level     : in Level_Type;
                             Logger    : in String;
                             Message   : in String;
                             Arguments : in String_Array_Access) is
   begin
      Strings.Formats.Format (Into, Message, Arguments);
   end Format_Message;

   --  ------------------------------
   --  Format the event into a string
   --  ------------------------------
   function Format_Event (Format  : in Formatter;
                          Layout  : in Layout_Type;
                          Date    : in Ada.Calendar.Time;
                          Use_UTC : in Boolean;
                          Level   : in Level_Type;
                          Logger  : in String) return String is
   begin
      case Layout is
         when MESSAGE =>
            return "";

         when LEVEL_MESSAGE =>
            return Get_Level_Name (Level) & ": ";

         when DATE_LEVEL_MESSAGE =>
            return "[" & Formatter'Class (Format).Format_Date (Use_UTC, Date) & "] "
              & Get_Level_Name (Level) & ": ";

         when FULL =>
            return "[" & Formatter'Class (Format).Format_Date (Use_UTC, Date)
              & "] " & Get_Level_Name (Level) & " - " & Logger & " - : ";

      end case;
   end Format_Event;

   function Format_Date (Format  : in Formatter;
                         Use_UTC : in Boolean;
                         Date    : in Ada.Calendar.Time) return String is
   begin
      if Use_UTC then
         return Ada.Calendar.Formatting.Image (Date);
      else
         return Ada.Calendar.Formatting.Image
           (Date, False,
            Ada.Calendar.Time_Zones.UTC_Time_Offset (Date));
      end if;
   end Format_Date;

   --  ------------------------------
   --  Create a formatter instance with a factory with the given name.
   --  ------------------------------
   function Create_Default (Name    : in String;
                            Config  : in Util.Properties.Manager) return Formatter_Access is
      pragma Unreferenced (Config);

      Result : constant Formatter_Access
        := new Formatter '(Ada.Finalization.Limited_Controlled with Length => Name'Length,
                           Name => Name,
                           others => <>);
   begin
      return Result.all'Access;
   end Create_Default;

   --  ------------------------------
   --  Create a formatter instance with a factory with the given name.
   --  ------------------------------
   function Create (Name    : in String;
                    Config  : in Util.Properties.Manager) return Formatter_Access is
      Prop_Name      : constant String := "formatter." & Name;
      Formatter_Type : constant String := Config.Get (Prop_Name, DEFAULT);
      Factory        : Formatter_Factory_Access := Formatter_Factories;
   begin
      while Factory /= null loop
         if Factory.Name = Formatter_Type then
            return Factory.Factory (Name, Config);
         end if;
         Factory := Factory.Next_Factory;
      end loop;

      Factory := Formatter_Factories;
      if Factory /= null then
         return Factory.Factory (Name, Config);
      end if;
      return null;
   end Create;

end Util.Log.Formatters;
