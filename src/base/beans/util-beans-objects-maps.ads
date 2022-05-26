-----------------------------------------------------------------------
--  util-beans-objects-maps -- Object maps
--  Copyright (C) 2010, 2011, 2012, 2017, 2018, 2022 Stephane Carrez
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Util.Beans.Basic;
with Util.Beans.Objects.Iterators;

--  == Object maps ==
--  The `Util.Beans.Objects.Maps` package provides a map of objects with a `String`
--  as key.  This allows to associated names to objects.
--  To create an instance of the map, it is possible to use the `Create` function
--  as follows:
--
--    with Util.Beans.Objects.Maps;
--    ...
--       Person : Util.Beans.Objects.Object := Util.Beans.Objects.Maps.Create;
--
--  Then, it becomes possible to populate the map with objects by using
--  the `Set_Value` procedure as follows:
--
--    Util.Beans.Objects.Set_Value (Person, "name",
--                                  To_Object (Name));
--    Util.Beans.Objects.Set_Value (Person, "last_name",
--                                  To_Object (Last_Name));
--    Util.Beans.Objects.Set_Value (Person, "age",
--                                  To_Object (Age));
--
--  Getting a value from the map is done by using the `Get_Value` function:
--
--    Name : Util.Beans.Objects.Object := Get_Value (Person, "name");
--
--  It is also possible to iterate over the values of the map by using
--  the `Iterate` procedure or by using the iterator support provided by
--  the `Util.Beans.Objects.Iterators` package.
package Util.Beans.Objects.Maps is

   subtype Iterator_Bean is Util.Beans.Objects.Iterators.Iterator_Bean;

   package Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Object,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   subtype Cursor is Maps.Cursor;
   subtype Map is Maps.Map;

   --  Make all the Maps operations available (a kind of 'use Maps' for anybody).
   function Length (Container : in Map) return Ada.Containers.Count_Type renames Maps.Length;
   function Is_Empty (Container : in Map) return Boolean renames Maps.Is_Empty;
   procedure Clear (Container : in out Map) renames Maps.Clear;
   function Key (Position : Cursor) return String renames Maps.Key;

   procedure Include (Container : in out Map;
                      Key : in String;
                      New_Item : in Object) renames Maps.Include;
   procedure Query_Element (Position : in Cursor;
                            Process  : not null access procedure (Key     : String;
                                                                  Element : Object))
                            renames Maps.Query_Element;

   function Has_Element (Position : Cursor) return Boolean renames Maps.Has_Element;
   function Element (Position : Cursor) return Object renames Maps.Element;
   procedure Next (Position : in out Cursor) renames Maps.Next;
   function Next (Position : Cursor) return Cursor renames Maps.Next;
   function Equivalent_Keys (Left, Right : Cursor) return Boolean renames Maps.Equivalent_Keys;
   function Equivalent_Keys (Left : Cursor; Right : String) return Boolean
                             renames Maps.Equivalent_Keys;
   function Equivalent_Keys (Left : String; Right : Cursor) return Boolean
                             renames Maps.Equivalent_Keys;
   function Copy (Source : Maps.Map; Capacity : in Ada.Containers.Count_Type) return Maps.Map
     renames Maps.Copy;

   --  ------------------------------
   --  Map Bean
   --  ------------------------------
   --  The <b>Map_Bean</b> is a map of objects that also exposes the <b>Bean</b> interface.
   --  This allows the map to be available and accessed from an Object instance.
   type Map_Bean is new Maps.Map and Util.Beans.Basic.Bean and Iterator_Bean with private;
   type Map_Bean_Access is access all Map_Bean'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Map_Bean;
                       Name : in String) return Object;

   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   overriding
   procedure Set_Value (From  : in out Map_Bean;
                        Name  : in String;
                        Value : in Object);

   --  Get an iterator to iterate starting with the first element.
   overriding
   function First (From : in Map_Bean) return Iterators.Proxy_Iterator_Access;

   --  Get an iterator to iterate starting with the last element.
   overriding
   function Last (From : in Map_Bean) return Iterators.Proxy_Iterator_Access;

   --  Create an object that contains a <tt>Map_Bean</tt> instance.
   function Create return Object;

   --  Iterate over the members of the map.
   procedure Iterate (From    : in Object;
                      Process : not null access procedure (Name : in String;
                                                           Item : in Object));

private

   type Map_Bean is new Maps.Map and Util.Beans.Basic.Bean
     and Iterator_Bean with null record;

   type Map_Iterator is new Iterators.Proxy_Map_Iterator with record
      Pos : Cursor;
   end record;
   type Map_Iterator_Access is access all Map_Iterator;

   overriding
   function Has_Element (Iter : in Map_Iterator) return Boolean;

   overriding
   procedure Next (Iter : in out Map_Iterator);

   overriding
   procedure Previous (Iter : in out Map_Iterator);

   overriding
   function Element (Iter : in Map_Iterator) return Object;

   overriding
   function Key (Iter : in Map_Iterator) return String;

end Util.Beans.Objects.Maps;
