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
with Util.Properties;
with Util.Strings.Builders;
with Util.Strings.Formats;
with Util.Named_Lists;
package Util.Log.Formatters is

   DEFAULT : constant String := "Default";

   package Lists is new Util.Named_Lists (Ada.Finalization.Limited_Controlled);

   subtype String_Array_Access is Util.Strings.Formats.String_Array_Access;

   type Formatter (Length : Positive) is
      new Lists.Named_Element (Length) with null record;
   type Formatter_Access is access all Formatter'Class;

   --  Format the message with the list of arguments.
   procedure Format (Format    : in Formatter;
                     Into      : in out Util.Strings.Builders.Builder;
                     Level     : in Level_Type;
                     Logger    : in String;
                     Message   : in String;
                     Arguments : in String_Array_Access);

   type Factory_Access is
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
