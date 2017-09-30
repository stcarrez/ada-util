-----------------------------------------------------------------------
--  util-beans-vectors -- Object vectors
--  Copyright (C) 2011, 2017 Stephane Carrez
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

with Ada.Containers.Vectors;
with Util.Beans.Basic;
package Util.Beans.Objects.Vectors is

   package Vectors is
     new Ada.Containers.Vectors (Index_Type       => Natural,
                                 Element_Type => Object);

   subtype Cursor is Vectors.Cursor;
   subtype Vector is Vectors.Vector;

   --  Make all the Vectors operations available (a kind of 'use Vectors' for anybody).
   function Length (Container : in Vector) return Ada.Containers.Count_Type renames Vectors.Length;
   function Is_Empty (Container : in Vector) return Boolean renames Vectors.Is_Empty;
   procedure Clear (Container : in out Vector) renames Vectors.Clear;
   function First (Container : in Vector) return Cursor renames Vectors.First;
   function Last (Container : in Vector) return Cursor renames Vectors.Last;
   function Element (Container : in Vector;
                     Position  : in Natural) return Object renames Vectors.Element;

   procedure Append (Container : in out Vector;
                     New_Item  : in Object;
                     Count     : in Ada.Containers.Count_Type := 1) renames Vectors.Append;
   procedure Query_Element (Position : in Cursor;
                            Process  : not null access procedure (Element : Object))
                            renames Vectors.Query_Element;
   procedure Update_Element (Container : in out Vector;
                             Position  : in Cursor;
                             Process   : not null access procedure (Element : in out Object))
                             renames Vectors.Update_Element;

   function Has_Element (Position : Cursor) return Boolean renames Vectors.Has_Element;
   function Element (Position : Cursor) return Object renames Vectors.Element;
   procedure Next (Position : in out Cursor) renames Vectors.Next;
   function Next (Position : Cursor) return Cursor renames Vectors.Next;
   function Previous (Position : Cursor) return Cursor renames Vectors.Previous;
   procedure Previous (Position : in out Cursor) renames Vectors.Previous;

   --  ------------------------------
   --  Map Bean
   --  ------------------------------
   --  The <b>Map_Bean</b> is a map of objects that also exposes the <b>Bean</b> interface.
   --  This allows the map to be available and accessed from an Object instance.
   type Vector_Bean is new Vectors.Vector and Util.Beans.Basic.Bean with private;
   type Vector_Bean_Access is access all Vector_Bean'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : in Vector_Bean;
                       Name : in String) return Object;

   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   procedure Set_Value (From  : in out Vector_Bean;
                        Name  : in String;
                        Value : in Object);

   --  Create an object that contains a <tt>Vector_Bean</tt> instance.
   function Create return Object;

private

   type Vector_Bean is new Vectors.Vector and Util.Beans.Basic.Bean with null record;

end Util.Beans.Objects.Vectors;
