-----------------------------------------------------------------------
--  util-concurrent-arrays -- Concurrent Arrays
--  Copyright (C) 2012, 2018 Stephane Carrez
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

package body Util.Concurrent.Arrays is

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => Vector_Record,
                                      Name   => Vector_Record_Access);

   --  ------------------------------
   --  Returns True if the container is empty.
   --  ------------------------------
   function Is_Empty (Container : in Ref) return Boolean is
   begin
      return Container.Target = null;
   end Is_Empty;

   --  ------------------------------
   --  Iterate over the vector elements and execute the <b>Process</b> procedure
   --  with the element as parameter.
   --  ------------------------------
   procedure Iterate (Container : in Ref;
                      Process   : not null access procedure (Item : in Element_Type)) is
      Target : constant Vector_Record_Access := Container.Target;
   begin
      if Target /= null then
         for I in Target.List'Range loop
            Process (Target.List (I));
         end loop;
      end if;
   end Iterate;

   --  ------------------------------
   --  Iterate over the vector elements in reverse order and execute the <b>Process</b> procedure
   --  with the element as parameter.
   --  ------------------------------
   procedure Reverse_Iterate (Container : in Ref;
                              Process   : not null access procedure (Item : in Element_Type)) is
      Target : constant Vector_Record_Access := Container.Target;
   begin
      if Target /= null then
         for I in reverse Target.List'Range loop
            Process (Target.List (I));
         end loop;
      end if;
   end Reverse_Iterate;

   --  ------------------------------
   --  Release the reference.  Invoke <b>Finalize</b> and free the storage if it was
   --  the last reference.
   --  ------------------------------
   overriding
   procedure Finalize (Obj : in out Ref) is
      Release : Boolean;
   begin
      if Obj.Target /= null then
         Util.Concurrent.Counters.Decrement (Obj.Target.Ref_Counter, Release);
         if Release then
            Free (Obj.Target);
         else
            Obj.Target := null;
         end if;
      end if;
   end Finalize;

   --  ------------------------------
   --  Update the reference counter after an assignment.
   --  ------------------------------
   overriding
   procedure Adjust (Obj : in out Ref) is
   begin
      if Obj.Target /= null then
         Util.Concurrent.Counters.Increment (Obj.Target.Ref_Counter);
      end if;
   end Adjust;

   --  ------------------------------
   --  Get a read-only reference to the vector elements.  The referenced vector will never
   --  be modified.
   --  ------------------------------
   function Get (Container : in Vector'Class) return Ref is
   begin
      return Container.List.Get;
   end Get;

   --  ------------------------------
   --  Append the element to the vector.  The modification will not be visible to readers
   --  until they call the <b>Get</b> function.
   --  ------------------------------
   procedure Append (Container : in out Vector;
                     Item      : in Element_Type) is
   begin
      Container.List.Append (Item);
   end Append;

   --  ------------------------------
   --  Remove the element represented by <b>Item</b> from the vector.  The modification will
   --  not be visible to readers until they call the <b>Get</b> function.
   --  ------------------------------
   procedure Remove (Container : in out Vector;
                     Item      : in Element_Type) is
   begin
      Container.List.Remove (Item);
   end Remove;

   --  Release the vector elements.
   overriding
   procedure Finalize (Object : in out Vector) is
   begin
      null;
   end Finalize;

   --  Vector of objects
   protected body Protected_Vector is

      --  ------------------------------
      --  Get a readonly reference to the vector.
      --  ------------------------------
      function Get return Ref is
      begin
         return Elements;
      end Get;

      --  ------------------------------
      --  Append the element to the vector.
      --  ------------------------------
      procedure Append (Item : in Element_Type) is
         New_Items : Vector_Record_Access;
         Len       : Natural;
      begin
         if Elements.Target = null then
            New_Items := new Vector_Record (Len => 1);

            Len := 1;
         else
            Len := Elements.Target.Len + 1;
            New_Items := new Vector_Record (Len => Len);
            New_Items.List (1 .. Len - 1) := Elements.Target.List;
            Finalize (Elements);
         end if;

         New_Items.List (Len) := Item;
         Util.Concurrent.Counters.Increment (New_Items.Ref_Counter);
         Elements.Target := New_Items;
      end Append;

      --  ------------------------------
      --  Remove the element from the vector.
      --  ------------------------------
      procedure Remove (Item : in Element_Type) is
         New_Items : Vector_Record_Access;
         Items     : constant Vector_Record_Access := Elements.Target;
      begin
         if Items = null then
            return;
         end if;
         for I in Items.List'Range loop
            if Items.List (I) = Item then
               if Items.Len = 1 then
                  Finalize (Elements);
                  Elements.Target := null;
               else
                  New_Items := new Vector_Record (Len => Items.Len - 1);
                  if I > 1 then
                     New_Items.List (1 .. I - 1) := Items.List (1 .. I - 1);
                  end if;
                  if I <= New_Items.List'Last then
                     New_Items.List (I .. New_Items.List'Last)
                       := Items.List (I + 1 .. Items.List'Last);
                  end if;
                  Finalize (Elements);
                  Util.Concurrent.Counters.Increment (New_Items.Ref_Counter);
                  Elements.Target := New_Items;
               end if;
               return;
            end if;
         end loop;
      end Remove;

   end Protected_Vector;

end Util.Concurrent.Arrays;
