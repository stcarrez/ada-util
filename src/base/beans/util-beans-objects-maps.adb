-----------------------------------------------------------------------
--  util-beans-objects-maps -- Object maps
--  Copyright (C) 2010, 2011, 2012, 2017, 2018, 2019, 2022 Stephane Carrez
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
   overriding
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
   overriding
   procedure Set_Value (From  : in out Map_Bean;
                        Name  : in String;
                        Value : in Object) is
   begin
      From.Include (Name, Value);
   end Set_Value;

   --  ------------------------------
   --  Get an iterator to iterate starting with the first element.
   --  ------------------------------
   overriding
   function First (From : in Map_Bean) return Iterators.Proxy_Iterator_Access is
      Iter : constant Map_Iterator_Access := new Map_Iterator;
   begin
      Iter.Pos := From.First;
      return Iter.all'Access;
   end First;

   --  ------------------------------
   --  Get an iterator to iterate starting with the last element.
   --  ------------------------------
   overriding
   function Last (From : in Map_Bean) return Iterators.Proxy_Iterator_Access is
      pragma Unreferenced (From);
   begin
      return null;
   end Last;

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

   function Get_Map is
      new Util.Beans.Objects.Iterators.Get_Bean (Map_Bean, Map_Bean_Access);

   overriding
   function Has_Element (Iter : in Map_Iterator) return Boolean is
      Map : constant Map_Bean_Access := Get_Map (Iter);
   begin
      return Map /= null and then Has_Element (Iter.Pos);
   end Has_Element;

   overriding
   procedure Next (Iter : in out Map_Iterator) is
   begin
      Next (Iter.Pos);
   end Next;

   overriding
   procedure Previous (Iter : in out Map_Iterator) is
   begin
      null;
   end Previous;

   overriding
   function Element (Iter : in Map_Iterator) return Object is
   begin
      return Element (Iter.Pos);
   end Element;

   overriding
   function Key (Iter : in Map_Iterator) return String is
   begin
      return Key (Iter.Pos);
   end Key;

end Util.Beans.Objects.Maps;
