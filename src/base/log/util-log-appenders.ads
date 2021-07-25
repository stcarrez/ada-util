-----------------------------------------------------------------------
--  util-log-appenders -- Log appenders
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
with Ada.Calendar;
with Ada.Finalization;
with Util.Properties;
with Util.Strings.Builders;

--  The log <b>Appender</b> will handle the low level operations to write
--  the log content to a file, the console, a database.
package Util.Log.Appenders is

   --  The layout type to indicate how to format the message.
   --  Unlike Logj4, there is no customizable layout.
   type Layout_Type
     is (
         --  The <b>message</b> layout with only the log message.
         --  Ex: "Cannot open file"
         MESSAGE,

         --  The <b>level-message</b> layout with level and message.
         --  Ex: "ERROR: Cannot open file"
         LEVEL_MESSAGE,

         --  The <b>date-level-message</b> layout with date
         --  Ex: "2011-03-04 12:13:34 ERROR: Cannot open file"
         DATE_LEVEL_MESSAGE,

         --  The <b>full</b> layout with everything (the default).
         --  Ex: "2011-03-04 12:13:34 ERROR - my.application - Cannot open file"
         FULL);

   type Appender (Length : Positive) is
     abstract new Ada.Finalization.Limited_Controlled with private;

   type Appender_Access is access all Appender'Class;

   --  Get the log level that triggers display of the log events
   function Get_Level (Self : in Appender) return Level_Type;

   --  Set the log level.
   procedure Set_Level (Self       : in out Appender;
                        Name       : in String;
                        Properties : in Util.Properties.Manager;
                        Level      : in Level_Type);
   procedure Set_Level (Self       : in out Appender;
                        Level      : in Level_Type);

   --  Set the log layout format.
   procedure Set_Layout (Self       : in out Appender;
                         Name       : in String;
                         Properties : in Util.Properties.Manager;
                         Layout     : in Layout_Type);
   procedure Set_Layout (Self       : in out Appender;
                         Layout     : in Layout_Type);

   --  Format the event into a string
   function Format (Self    : in Appender'Class;
                    Date    : in Ada.Calendar.Time;
                    Level   : in Level_Type;
                    Logger  : in String) return String;

   --  Append a log event to the appender.  Depending on the log level
   --  defined on the appender, the event can be taken into account or
   --  ignored.
   procedure Append (Self    : in out Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String) is abstract;

   --  Flush the log events.
   procedure Flush (Self   : in out Appender) is abstract;

   --  ------------------------------
   --  List appender
   --  ------------------------------
   --  Write log events to a list of appenders
   type List_Appender (Length : Positive) is new Appender with private;
   type List_Appender_Access is access all List_Appender'Class;

   --  Max number of appenders that can be added to the list.
   --  In most cases, 2 or 3 appenders will be used.
   MAX_APPENDERS : constant Natural := 10;

   overriding
   procedure Append (Self    : in out List_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Level_Type;
                     Logger  : in String);

   --  Flush the log events.
   overriding
   procedure Flush (Self   : in out List_Appender);

   --  Add the appender to the list.
   procedure Add_Appender (Self   : in out List_Appender;
                           Object : in Appender_Access);

   --  Create a list appender and configure it according to the properties
   function Create_List_Appender (Name : in String) return List_Appender_Access;

   type Appender_List is limited private;

   --  Find an appender with a given name from the list of appenders.
   --  Returns null if there is no such appender.
   function Find_Appender (List : in Appender_List;
                           Name : in String) return Appender_Access;

   --  Add the appender to the list of appenders.
   procedure Add_Appender (List     : in out Appender_List;
                           Appender : in Appender_Access);

   --  Clear the list of appenders.
   procedure Clear (List : in out Appender_List);

   type Factory_Access is
     access function (Name       : in String;
                      Properties : in Util.Properties.Manager;
                      Default    : in Level_Type) return Appender_Access;

   type Appender_Factory;
   type Appender_Factory_Access is access all Appender_Factory;

   type Appender_Factory (Length : Positive) is limited record
      Next_Factory : Appender_Factory_Access;
      Factory      : Factory_Access;
      Name         : String (1 .. Length);
   end record;

   --  Register the factory handler to create an appender instance.
   procedure Register (Into   : in Appender_Factory_Access;
                       Name   : in String;
                       Create : in Factory_Access);

   --  Create an appender instance with a factory with the given name.
   function Create (Name    : in String;
                    Config  : in Util.Properties.Manager;
                    Default : in Level_Type) return Appender_Access;

private

   --  ------------------------------
   --  Log appender
   --  ------------------------------
   type Appender (Length : Positive) is abstract
     new Ada.Finalization.Limited_Controlled with record
      Next     : Appender_Access;
      Level    : Level_Type := INFO_LEVEL;
      Layout   : Layout_Type := FULL;
      Name     : String (1 .. Length);
   end record;

   type Appender_List is limited record
      First : Appender_Access;
   end record;

   type Appender_Array_Access is array (1 .. MAX_APPENDERS) of Appender_Access;

   type List_Appender (Length : Positive) is new Appender (Length) with record
      Appenders : Appender_Array_Access;
      Count     : Natural := 0;
   end record;

end Util.Log.Appenders;
