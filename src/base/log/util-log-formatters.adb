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
package body Util.Log.Formatters is

   procedure Format (Format    : in Formatter;
                     Into      : in out Util.Strings.Builders.Builder;
                     Level     : in Level_Type;
                     Logger    : in String;
                     Message   : in String;
                     Arguments : in String_Array_Access) is
   begin
      Strings.Formats.Format (Into, Message, Arguments);
   end Format;

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
