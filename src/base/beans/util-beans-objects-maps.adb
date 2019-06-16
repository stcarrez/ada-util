-----------------------------------------------------------------------
--  util-beans-objects-maps -- Object maps
--  Copyright (C) 2010, 2011, 2012, 2017, 2018, 2019 Stephane Carrez
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

package body Util.Beans.Objects.Maps is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : in Map_Bean;
                       Name : in String) return Object is
      Pos : constant Cursor := From.Find (Name);
   begin
      if Has_Element (Pos) then
         return Element (Pos);
      else
         return Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   --  ------------------------------
   procedure Set_Value (From  : in out Map_Bean;
                        Name  : in String;
                        Value : in Object) is
   begin
      From.Include (Name, Value);
   end Set_Value;

   --  ------------------------------
   --  Iterate over the members of the map.
   --  ------------------------------
   procedure Iterate (From    : in Object;
                      Process : not null access procedure (Name : in String;
                                                           Item : in Object)) is
      procedure Process_One (Pos : in Maps.Cursor);

      procedure Process_One (Pos : in Maps.Cursor) is
      begin
         Process (Maps.Key (Pos), Maps.Element (Pos));
      end Process_One;

      Bean : constant access Util.Beans.Basic.Readonly_Bean'Class := To_Bean (From);
   begin
      if Bean /= null and then Bean.all in Util.Beans.Objects.Maps.Map_Bean'Class then
         Map_Bean'Class (Bean.all).Iterate (Process_One'Access);
      end if;
   end Iterate;

   --  ------------------------------
   --  Create an object that contains a <tt>Map_Bean</tt> instance.
   --  ------------------------------
   function Create return Object is
      M : constant Map_Bean_Access := new Map_Bean;
   begin
      return To_Object (Value => M, Storage => DYNAMIC);
   end Create;

end Util.Beans.Objects.Maps;
