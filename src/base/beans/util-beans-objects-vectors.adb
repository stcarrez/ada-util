-----------------------------------------------------------------------
--  util-beans-vectors -- Object vectors
--  Copyright (C) 2011, 2017, 2019, 2022 Stephane Carrez
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

package body Util.Beans.Objects.Vectors is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Vector_Bean;
                       Name : in String) return Object is
   begin
      if Name = "count" then
         return To_Object (Natural (From.Length));
      else
         return Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : in Vector_Bean) return Natural is
   begin
      return Natural (From.Length);
   end Get_Count;

   --  ------------------------------
   --  Get the element at the given position.
   --  ------------------------------
   overriding
   function Get_Row (From     : in Vector_Bean;
                     Position : in Natural) return Util.Beans.Objects.Object is
   begin
      return From.Element (Position);
   end Get_Row;

   --  -----------------------
   --  Create an object that contains a <tt>Vector_Bean</tt> instance.
   --  -----------------------
   function Create return Object is
      M : constant Vector_Bean_Access := new Vector_Bean;
   begin
      return To_Object (Value => M, Storage => DYNAMIC);
   end Create;

   --  -----------------------
   --  Iterate over the vectors or array elements.
   --  If the object is not a `Vector_Bean` or an array, the operation does nothing.
   --  -----------------------
   procedure Iterate (From    : in Object;
                      Process : not null access procedure (Position : in Positive;
                                                           Item     : in Object)) is
      procedure Process_One (Pos : in Vectors.Cursor);

      procedure Process_One (Pos : in Vectors.Cursor) is
      begin
         Process (Vectors.To_Index (Pos), Vectors.Element (Pos));
      end Process_One;

      Bean : constant access Util.Beans.Basic.Readonly_Bean'Class := To_Bean (From);
   begin
      if Bean /= null and then Bean.all in Vector_Bean'Class then
         Vector_Bean'Class (Bean.all).Iterate (Process_One'Access);

      elsif Is_Array (From) then
         declare
            Count : constant Natural := Get_Count (From);
         begin
            for Pos in 1 .. Count loop
               Process (Pos, Get_Value (From, Pos));
            end loop;
         end;
      end if;
   end Iterate;

   --  -----------------------
   --  Get an iterator to iterate starting with the first element.
   --  -----------------------
   overriding
   function First (From : in Vector_Bean) return Iterators.Proxy_Iterator_Access is
      Iter : constant Vector_Iterator_Access := new Vector_Iterator;
   begin
      Iter.Pos := From.First;
      return Iter.all'Access;
   end First;

   --  -----------------------
   --  Get an iterator to iterate starting with the last element.
   --  -----------------------
   overriding
   function Last (From : in Vector_Bean) return Iterators.Proxy_Iterator_Access is
      Iter : constant Vector_Iterator_Access := new Vector_Iterator;
   begin
      Iter.Pos := From.Last;
      return Iter.all'Access;
   end Last;

   overriding
   function Has_Element (Iter : in Vector_Iterator) return Boolean is
   begin
      return Vectors.Has_Element (Iter.Pos);
   end Has_Element;

   overriding
   procedure Next (Iter : in out Vector_Iterator) is
   begin
      Vectors.Next (Iter.Pos);
   end Next;

   overriding
   procedure Previous (Iter : in out Vector_Iterator) is
   begin
      Vectors.Previous (Iter.Pos);
   end Previous;

   overriding
   function Element (Iter : in Vector_Iterator) return Object is
   begin
      return Vectors.Element (Iter.Pos);
   end Element;

end Util.Beans.Objects.Vectors;
