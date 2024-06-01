-----------------------------------------------------------------------
--  util-log-formatters -- Log formatter
--  Copyright (C) 2024 Stephane Carrez
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
with Ada.Calendar;
with Util.Properties;
with Util.Strings.Builders;
with Util.Strings.Formats;
with Util.Named_Lists;
package Util.Log.Formatters is

   DEFAULT : constant String := "Default";

   --  The layout type to indicate how to format the message.
   --  Unlike Logj4, there is no customizable layout.
   type Layout_Type
     is (
         --  The `message` layout with only the log message.
         --  Ex: "Cannot open file"
         MESSAGE,

         --  The `level-message` layout with level and message.
         --  Ex: "ERROR: Cannot open file"
         LEVEL_MESSAGE,

         --  The `date-level-message` layout with date
         --  Ex: "2011-03-04 12:13:34 ERROR: Cannot open file"
         DATE_LEVEL_MESSAGE,

         --  The `full` layout with everything (the default).
         --  Ex: "2011-03-04 12:13:34 ERROR - my.application - Cannot open file"
         FULL);

   package Lists is new Util.Named_Lists (Ada.Finalization.Limited_Controlled);

   subtype String_Array_Access is Util.Strings.Formats.String_Array_Access;

   type Formatter (Length : Positive) is
      new Lists.Named_Element (Length) with null record;
   type Formatter_Access is access all Formatter'Class;

   --  Format the message with the list of arguments.
   procedure Format_Message (Format    : in Formatter;
                             Into      : in out Util.Strings.Builders.Builder;
                             Level     : in Level_Type;
                             Logger    : in String;
                             Message   : in String;
                             Arguments : in String_Array_Access);

   --  Format the event into a string
   function Format_Event (Format  : in Formatter;
                          Layout  : in Layout_Type;
                          Date    : in Ada.Calendar.Time;
                          Use_UTC : in Boolean;
                          Level   : in Level_Type;
                          Logger  : in String) return String;

   --  Format the date into a string.
   function Format_Date (Format  : in Formatter;
                         Use_UTC : in Boolean;
                         Date    : in Ada.Calendar.Time) return String;

   type Factory_Access is not null
     access function (Name       : in String;
                      Properties : in Util.Properties.Manager) return Formatter_Access;

   --  Create a formatter instance with a factory with the given name.
   function Create (Name    : in String;
                    Config  : in Util.Properties.Manager) return Formatter_Access;

   --  Create a formatter instance with a factory with the given name.
   function Create_Default (Name    : in String;
                            Config  : in Util.Properties.Manager) return Formatter_Access;

private

   type Formatter_Factory;
   type Formatter_Factory_Access is access all Formatter_Factory;

   Formatter_Factories : Formatter_Factory_Access;

   type Formatter_Factory (Length       : Positive;
                           Factory      : Factory_Access;
                           Next_Factory : Formatter_Factory_Access) is limited
   record
      Name : String (1 .. Length);
   end record;

end Util.Log.Formatters;
