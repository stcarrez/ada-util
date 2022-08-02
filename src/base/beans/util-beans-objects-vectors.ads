-----------------------------------------------------------------------
--  util-beans-vectors -- Object vectors
--  Copyright (C) 2011, 2017, 2022 Stephane Carrez
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
with Util.Beans.Objects.Iterators;

--  == Object vectors ==
--  The `Util.Beans.Objects.Vectors` package provides a vector of objects.
--  To create an instance of the vector, it is possible to use the `Create` function
--  as follows:
--
--    with Util.Beans.Objects.Vectors;
--    ...
--       List : Util.Beans.Objects.Object := Util.Beans.Objects.Vectors.Create;
--
package Util.Beans.Objects.Vectors is

   subtype Iterator_Bean is Util.Beans.Objects.Iterators.Iterator_Bean;

   package Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
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
   --  Vector Bean
   --  ------------------------------
   --  The `Vector_Bean` is a vector of objects that also exposes the <b>Bean</b> interface.
   --  This allows the vector to be available and accessed from an Object instance.
   type Vector_Bean is new Vectors.Vector and Util.Beans.Basic.Array_Bean
     and Iterator_Bean with private;
   type Vector_Bean_Access is access all Vector_Bean'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Vector_Bean;
                       Name : in String) return Object;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : in Vector_Bean) return Natural;

   --  Get the element at the given position.
   overriding
   function Get_Row (From     : in Vector_Bean;
                     Position : in Natural) return Util.Beans.Objects.Object;

   --  Get an iterator to iterate starting with the first element.
   overriding
   function First (From : in Vector_Bean) return Iterators.Proxy_Iterator_Access;

   --  Get an iterator to iterate starting with the last element.
   overriding
   function Last (From : in Vector_Bean) return Iterators.Proxy_Iterator_Access;

   --  Create an object that contains a `Vector_Bean` instance.
   function Create return Object;

   --  Iterate over the vectors or array elements.
   --  If the object is not a `Vector_Bean` or an array, the operation does nothing.
   procedure Iterate (From    : in Object;
                      Process : not null access procedure (Position : in Positive;
                                                           Item     : in Object));

private

   type Vector_Bean is new Vectors.Vector and Util.Beans.Basic.Array_Bean
     and Iterator_Bean with null record;

   type Vector_Iterator is new Iterators.Proxy_Iterator with record
      Pos : Cursor;
   end record;
   type Vector_Iterator_Access is access all Vector_Iterator;

   overriding
   function Has_Element (Iter : in Vector_Iterator) return Boolean;

   overriding
   procedure Next (Iter : in out Vector_Iterator);

   overriding
   procedure Previous (Iter : in out Vector_Iterator);

   overriding
   function Element (Iter : in Vector_Iterator) return Object;

end Util.Beans.Objects.Vectors;
