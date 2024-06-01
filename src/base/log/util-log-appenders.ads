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
with Ada.Calendar;
with Ada.Finalization;
with Util.Properties;
with Util.Strings.Builders;
with Util.Named_Lists;
with Util.Log.Formatters;

--  The log `Appender` will handle the low level operations to write
--  the log content to a file, the console, a database.
package Util.Log.Appenders is

   subtype Layout_Type is Util.Log.Formatters.Layout_Type;
   use all type Util.Log.Formatters.Layout_Type;

   package Lists is new Util.Named_Lists (Ada.Finalization.Limited_Controlled);

   subtype Formatter_Access is not null Util.Log.Formatters.Formatter_Access;

   type Appender (Length    : Positive;
                  Formatter : Formatter_Access) is abstract
     new Lists.Named_Element (Length) with record
      Level     : Level_Type := INFO_LEVEL;
      Layout    : Layout_Type := FULL;
      Use_UTC   : Boolean := False;
   end record;

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
   type List_Appender (Length    : Positive;
                       Formatter : Formatter_Access) is new Appender with private;
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
   function Create_List_Appender (Name      : in String;
                                  Formatter : in Formatter_Access) return List_Appender_Access;

   type Factory_Access is not null
     access function (Name       : in String;
                      Formatter  : in Formatter_Access;
                      Properties : in Util.Properties.Manager;
                      Default    : in Level_Type) return Appender_Access;

   --  Create an appender instance with a factory with the given name.
   function Create (Name      : in String;
                    Formatter : in Formatter_Access;
                    Config    : in Util.Properties.Manager;
                    Default   : in Level_Type) return Appender_Access;

private

   type Appender_Factory;
   type Appender_Factory_Access is access all Appender_Factory;

   type Appender_Factory (Length       : Positive;
                          Factory      : Factory_Access;
                          Next_Factory : Appender_Factory_Access) is limited
   record
      Name : String (1 .. Length);
   end record;

   Appender_Factories : Appender_Factory_Access;

   type Appender_Array_Access is array (1 .. MAX_APPENDERS) of Appender_Access;

   type List_Appender (Length    : Positive;
                       Formatter : Formatter_Access) is new Appender (Length, Formatter) with
   record
      Appenders : Appender_Array_Access;
      Count     : Natural := 0;
   end record;

end Util.Log.Appenders;
