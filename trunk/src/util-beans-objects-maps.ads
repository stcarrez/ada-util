-----------------------------------------------------------------------
--  Util.Beans.Objects.Maps -- Object maps
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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
package Util.Beans.Objects.Maps is

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

   --  ------------------------------
   --  Map Bean
   --  ------------------------------
   --  The <b>Map_Bean</b> is a map of objects that also exposes the <b>Bean</b> interface.
   --  This allows the map to be available and accessed from an Object instance.
   type Map_Bean is new Maps.Map and Util.Beans.Basic.Bean with private;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : in Map_Bean;
                       Name : in String) return Object;

   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   procedure Set_Value (From  : in out Map_Bean;
                        Name  : in String;
                        Value : in Object);

private

   type Map_Bean is new Maps.Map and Util.Beans.Basic.Bean with null record;

end Util.Beans.Objects.Maps;
