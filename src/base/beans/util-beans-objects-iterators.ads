-----------------------------------------------------------------------
--  util-beans-objects-iterators -- Iterators for objects
--  Copyright (C) 2022 Stephane Carrez
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
with Ada.Iterator_Interfaces;
with Util.Beans.Basic;

--  == Object iterator ==
--  Iterators are provided by the `Util.Beans.Objects.Iterators` package.
--  The iterator instance is created by using either the `First` or `Last`
--  function on the object to iterate.
--
--    with Util.Beans.Objects.Iterators;
--    ...
--       Iter : Util.Beans.Objects.Iterators.Iterator
--          := Util.Beans.Objects.Iterators.First (Object);
--
--  The iterator is used in conjunction with its `Has_Element` function
--  and either its `Next` or `Previous` procedure.  The current element
--  is obtained by using the `Element` function.  When the object being
--  iterated is a map, a key can be associated with the element and
--  is obtained by the `Key` function.
--
--    while Util.Beans.Objects.Iterators.Has_Element (Iter) loop
--       declare
--          Item : Object := Util.Beans.Objects.Iterators.Element (Iter);
--          Key  : String := Util.Beans.Objects.Iterators.Key (Iter);
--       begin
--          ...
--          Util.Beans.Objects.Iterators.Next (Iter);
--       end;
--    end loop;
--
package Util.Beans.Objects.Iterators is

   type Iterator is tagged private;

   --  Returns True if the iterator provides a key for each element.
   function Has_Key (Iter : in Iterator) return Boolean;

   --  Returns True if the iterator has an element.
   function Has_Element (Iter : in Iterator) return Boolean;

   --  Returns the current iterator element or Null_Object if it is empty.
   function Element (Iter : in Iterator) return Object;

   --  Returns the key associated with the current element.
   function Key (Iter : in Iterator) return String;

   --  Move the iterator to the next element.
   procedure Next (Iter : in out Iterator);

   --  Move the iterator to the previous element.
   procedure Previous (Iter : in out Iterator);

   --  Create an iterator to iterate from the first element.
   function First (Item : in Object) return Iterator;

   --  Create an iterator to iterate from the last element.
   function Last (Item : in Object) return Iterator;

   package Object_Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Iterator, Has_Element);

   function Iterate (Item : in Object) return
            Object_Iterator_Interfaces.Forward_Iterator'Class;

   --  ------------------------------
   --  Proxy_Iterator
   --  ------------------------------
   --  The `Proxy_Iterator` is the base type for the implementation of an iterator.
   --  An instance is created by the `Iterator_Bean` interface.
   --  The `Porxy_Map_Iterator` extends the iterator to provide access to
   --  the key when the iterator gives access to the elements of a map.
   type Proxy_Iterator is abstract new Ada.Finalization.Limited_Controlled with private;
   type Proxy_Iterator_Access is access all Proxy_Iterator'Class;

   overriding
   procedure Finalize (Proxy : in out Proxy_Iterator);

   function Has_Element (Iter : in Proxy_Iterator) return Boolean is abstract;

   procedure Next (Iter : in out Proxy_Iterator) is abstract;

   procedure Previous (Iter : in out Proxy_Iterator) is abstract;

   function Element (Iter : in Proxy_Iterator) return Object is abstract;

   type Proxy_Map_Iterator is abstract new Proxy_Iterator with private;
   type Proxy_Map_Iterator_Access is access all Proxy_Map_Iterator'Class;

   function Key (Iter : in Proxy_Map_Iterator) return String is abstract;

   generic
      type T is limited new Util.Beans.Basic.Readonly_Bean with private;
      type T_Access is access all T'Class;
   function Get_Bean (Iter : in Proxy_Iterator'Class) return T_Access;

   --  ------------------------------
   --  Iterator
   --  ------------------------------
   --  The `Iterator_Bean` interface allows to create an iterator on the bean object
   --  to iterate over its internal values.  The `First` and `Last` function must
   --  create an instance of a `Proxy_Iterator` interface which provides the
   --  operations to iterate.
   type Iterator_Bean is limited interface;
   type Iterator_Bean_Access is access all Iterator_Bean'Class;

   --  Get an iterator to iterate starting with the first element.
   function First (From : in Iterator_Bean) return Proxy_Iterator_Access is abstract;

   --  Get an iterator to iterate starting with the last element.
   function Last (From : in Iterator_Bean) return Proxy_Iterator_Access is abstract;

private

   type Iterator is new Controlled with record
      Iter : Proxy_Iterator_Access;
   end record;

   overriding
   procedure Adjust (Iter : in out Iterator);

   overriding
   procedure Finalize (Iter : in out Iterator);

   type Proxy_Iterator is abstract new Ada.Finalization.Limited_Controlled with record
      Ref_Counter : Util.Concurrent.Counters.Counter;
      Proxy       : Bean_Proxy_Access;
   end record;

   type Proxy_Map_Iterator is abstract new Proxy_Iterator with null record;

   package Iter is

      type Forward_Iterator is limited new Object_Iterator_Interfaces.Forward_Iterator with record
         Iter : Iterator;
      end record;

      overriding
      function First (Object : in Forward_Iterator) return Iterator;

      overriding
      function Next (Object : in Forward_Iterator;
                     Pos    : in Iterator) return Iterator;

   end Iter;

end Util.Beans.Objects.Iterators;
