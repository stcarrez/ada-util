-----------------------------------------------------------------------
--  Util.Beans.Objects.Pairs -- Pairs of objects
--  Copyright (C) 2013, 2022 Stephane Carrez
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

package body Util.Beans.Objects.Pairs is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Pair;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "key" or else Name = "first" then
         return From.First;
      elsif Name = "value" or else Name = "second" then
         return From.Second;
      else
         return Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Pair;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "key" or else Name = "first" then
         From.First := Value;
      elsif Name = "value" or else Name = "second" then
         From.Second := Value;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Return an object represented by the pair of two values.
   --  ------------------------------
   function To_Object (First, Second : in Object) return Object is
      Result : constant Pair_Access := new Pair;
   begin
      Result.First  := First;
      Result.Second := Second;
      return To_Object (Result.all'Access);
   end To_Object;

end Util.Beans.Objects.Pairs;
