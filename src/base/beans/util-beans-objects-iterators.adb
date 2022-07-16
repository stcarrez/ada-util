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
with Ada.Unchecked_Deallocation;

package body Util.Beans.Objects.Iterators is

   --  ------------------------------
   --  Returns True if the iterator provides a key for each element.
   --  ------------------------------
   function Has_Key (Iter : in Iterator) return Boolean is
   begin
      return Iter.Iter /= null and then Iter.Iter.all in Proxy_Map_Iterator'Class;
   end Has_Key;

   --  ------------------------------
   --  Returns True if the iterator has an element.
   --  ------------------------------
   function Has_Element (Iter : in Iterator) return Boolean is
   begin
      return Iter.Iter /= null and then Iter.Iter.Has_Element;
   end Has_Element;

   --  ------------------------------
   --  Returns the current iterator element or Null_Object if it is empty.
   --  ------------------------------
   function Element (Iter : in Iterator) return Object is
   begin
      if Iter.Iter = null then
         return Null_Object;
      else
         return Iter.Iter.Element;
      end if;
   end Element;

   --  ------------------------------
   --  Returns the key associated with the current element.
   --  ------------------------------
   function Key (Iter : in Iterator) return String is
   begin
      if Iter.Iter = null then
         return "";
      elsif not (Iter.Iter.all in Proxy_Map_Iterator'Class) then
         return "";
      else
         return Proxy_Map_Iterator'Class (Iter.Iter.all).Key;
      end if;
   end Key;

   --  ------------------------------
   --  Move the iterator to the next element.
   --  ------------------------------
   procedure Next (Iter : in out Iterator) is
   begin
      if Iter.Iter /= null then
         Iter.Iter.Next;
      end if;
   end Next;

   --  ------------------------------
   --  Move the iterator to the previous element.
   --  ------------------------------
   procedure Previous (Iter : in out Iterator) is
   begin
      if Iter.Iter /= null then
         Iter.Iter.Previous;
      end if;
   end Previous;

   overriding
   procedure Adjust (Iter : in out Iterator) is
   begin
      if Iter.Iter /= null then
         Util.Concurrent.Counters.Increment (Iter.Iter.Ref_Counter);
      end if;
   end Adjust;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Proxy_Iterator'Class,
                                     Name   => Proxy_Iterator_Access);

   overriding
   procedure Finalize (Iter : in out Iterator) is
      Release : Boolean;
   begin
      if Iter.Iter /= null then
         Util.Concurrent.Counters.Decrement (Iter.Iter.Ref_Counter, Release);
         if Release then
            Free (Iter.Iter);
         else
            Iter.Iter := null;
         end if;
      end if;
   end Finalize;

   --  ------------------------------
   --  Create an iterator to iterate from the first element.
   --  ------------------------------
   function First (Item : in Object) return Iterator is
      Iter : Iterator;
   begin
      if Item.V.Of_Type /= TYPE_BEAN then
         return Iter;
      end if;
      if Item.V.Proxy = null then
         return Iter;
      end if;
      declare
         Proxy       : constant Bean_Proxy_Access := Item.V.Proxy;
         List        : Iterator_Bean_Access;
      begin
         if not (Bean_Proxy (Proxy.all).Bean.all in Iterator_Bean'Class) then
            return Iter;
         end if;
         List := Iterator_Bean'Class (Bean_Proxy (Proxy.all).Bean.all)'Unchecked_Access;
         Iter.Iter := List.First;
         if Iter.Iter /= null then
            Util.Concurrent.Counters.Increment (Iter.Iter.Ref_Counter);
            Iter.Iter.Proxy := Proxy;
            Util.Concurrent.Counters.Increment (Proxy.Ref_Counter);
         end if;
      end;
      return Iter;
   end First;

   --  ------------------------------
   --  Create an iterator to iterate from the last element.
   --  ------------------------------
   function Last (Item : in Object) return Iterator is
      Iter : Iterator;
   begin
      if Item.V.Of_Type /= TYPE_BEAN then
         return Iter;
      end if;
      if Item.V.Proxy = null then
         return Iter;
      end if;
      declare
         Proxy       : constant Bean_Proxy_Access := Item.V.Proxy;
         List        : Iterator_Bean_Access;
      begin
         if not (Bean_Proxy (Proxy.all).Bean.all in Iterator_Bean'Class) then
            return Iter;
         end if;
         List := Iterator_Bean'Class (Bean_Proxy (Proxy.all).Bean.all)'Unchecked_Access;
         Iter.Iter := List.Last;
         if Iter.Iter /= null then
            Util.Concurrent.Counters.Increment (Iter.Iter.Ref_Counter);
            Iter.Iter.Proxy := Proxy;
            Util.Concurrent.Counters.Increment (Proxy.Ref_Counter);
         end if;
      end;
      return Iter;
   end Last;

   package body Iter is

      overriding
      function First (Object : in Forward_Iterator) return Iterator is
      begin
         return Object.Iter;
      end First;

      overriding
      function Next (Object : in Forward_Iterator;
                     Pos    : in Iterator) return Iterator is
         pragma Unreferenced (Object);

         Result : Iterator := Pos;
      begin
         Next (Result);
         return Result;
      end Next;

   end Iter;

   function Iterate (Item : in Object) return
     Object_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Result : Iter.Forward_Iterator do
         Result.Iter := First (Item);
      end return;
   end Iterate;

   overriding
   procedure Finalize (Proxy : in out Proxy_Iterator) is
      Release : Boolean;
   begin
      if Proxy.Proxy /= null then
         Util.Concurrent.Counters.Decrement (Proxy.Proxy.Ref_Counter, Release);
         if Release then
            Free (Proxy.Proxy);
         else
            Proxy.Proxy := null;
         end if;
      end if;
   end Finalize;

   function Get_Bean (Iter : in Proxy_Iterator'Class) return T_Access is
   begin
      if Iter.Proxy = null then
         return null;
      end if;
      if not (Bean_Proxy (Iter.Proxy.all).Bean.all in T'Class) then
         return null;
      end if;
      return T'Class (Bean_Proxy (Iter.Proxy.all).Bean.all)'Unchecked_Access;
   end Get_Bean;

end Util.Beans.Objects.Iterators;
