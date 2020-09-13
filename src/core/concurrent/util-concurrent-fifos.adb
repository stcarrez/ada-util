-----------------------------------------------------------------------
--  util-concurrent-fifos -- Concurrent Fifo Queues
--  Copyright (C) 2012, 2014, 2015, 2017, 2020 Stephane Carrez
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
package body Util.Concurrent.Fifos is

   --  ------------------------------
   --  Put the element in the queue.
   --  ------------------------------
   procedure Enqueue (Into    : in out Fifo;
                      Item    : in Element_Type;
                      Wait    : in Duration := FOREVER) is
   begin
      if Wait < 0.0 then
         Into.Buffer.Enqueue (Item);
      else
         select
            Into.Buffer.Enqueue (Item);
         or
            delay Wait;
            raise Timeout;
         end select;
      end if;
   end Enqueue;

   --  ------------------------------
   --  Get an element from the queue.
   --  Wait until one element gets available.
   --  ------------------------------
   procedure Dequeue (From    : in out Fifo;
                      Item    : out Element_Type;
                      Wait    : in Duration := FOREVER) is
   begin
      if Wait < 0.0 then
         From.Buffer.Dequeue (Item);
      else
         select
            From.Buffer.Dequeue (Item);
         or
            delay Wait;
            raise Timeout;
         end select;
      end if;
   end Dequeue;

   --  ------------------------------
   --  Wait for the fifo to become empty.
   --  ------------------------------
   procedure Wait_Empty (From : in out Fifo) is
   begin
      From.Buffer.Wait_Empty;
   end Wait_Empty;

   --  ------------------------------
   --  Get the number of elements in the queue.
   --  ------------------------------
   function Get_Count (From : in Fifo) return Natural is
   begin
      return From.Buffer.Get_Count;
   end Get_Count;

   --  ------------------------------
   --  Set the queue size.
   --  ------------------------------
   procedure Set_Size (Into : in out Fifo;
                       Capacity : in Positive) is
   begin
      Into.Buffer.Set_Size (Capacity);
   end Set_Size;

   --  ------------------------------
   --  Initializes the queue.
   --  ------------------------------
   overriding
   procedure Initialize (Object : in out Fifo) is
   begin
      Object.Buffer.Set_Size (Default_Size);
   end Initialize;

   --  ------------------------------
   --  Release the queue elements.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Fifo) is
   begin
      if Clear_On_Dequeue then
         while Object.Get_Count > 0 loop
            declare
               Unused : Element_Type;
            begin
               Object.Dequeue (Unused);
            end;
         end loop;
      end if;
      Object.Buffer.Set_Size (0);
   end Finalize;

   --  Queue of objects.
   protected body Protected_Fifo is

      --  ------------------------------
      --  Put the element in the queue.
      --  If the queue is full, wait until some room is available.
      --  ------------------------------
      entry Enqueue (Item : in Element_Type) when
        Count < Elements'Length - (if Clear_On_Dequeue then 1 else 0) is
      begin
         Elements (Last) := Item;
         Last := Last + 1;
         if Last > Elements'Last then
            if Clear_On_Dequeue then
               Last := Elements'First + 1;
            else
               Last := Elements'First;
            end if;
         end if;
         Count := Count + 1;
      end Enqueue;

      --  ------------------------------
      --  Get an element from the queue.
      --  Wait until one element gets available.
      --  ------------------------------
      entry Dequeue (Item : out Element_Type) when Count > 0 is
      begin
         Count := Count - 1;
         Item := Elements (First);

         --  For the clear on dequeue mode, erase the queue element.
         --  If the element holds some storage or a reference, this gets cleared here.
         --  The element used to clear is at index 0 (which does not exist if Clear_On_Dequeue
         --  is false).  There is no overhead when this is not used
         --  (ie, instantiation/compile time flag).
         if Clear_On_Dequeue then
            Elements (First) := Elements (0);
         end if;
         First := First + 1;
         if First > Elements'Last then
            if Clear_On_Dequeue then
               First := Elements'First + 1;
            else
               First := Elements'First;
            end if;
         end if;
      end Dequeue;

      --  ------------------------------
      --  Wait for the queue to become empty.
      --  ------------------------------
      entry Wait_Empty when Count = 0 is
      begin
         null;
      end Wait_Empty;

      --  ------------------------------
      --  Get the number of elements in the queue.
      --  ------------------------------
      function Get_Count return Natural is
      begin
         return Count;
      end Get_Count;

      --  ------------------------------
      --  Set the queue size.
      --  ------------------------------
      procedure Set_Size (Capacity : in Natural) is
         procedure Free is new Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);
         First_Pos : Natural := 1;
      begin
         if Clear_On_Dequeue then
            First_Pos := 0;
         end if;
         if Capacity = 0 then
            Free (Elements);
         elsif Elements = null then
            Elements := new Element_Array (First_Pos .. Capacity);
         else
            declare
               New_Array : constant Element_Array_Access
                 := new Element_Array (First_Pos .. Capacity);
            begin
               if Capacity > Elements'Length then
                  New_Array (First_Pos .. Elements'Last) := Elements (First_Pos .. Elements'Last);
               else
                  New_Array (First_Pos .. Capacity) := Elements (First_Pos .. Capacity);
               end if;

               Free (Elements);
               Elements := New_Array;
            end;
         end if;
      end Set_Size;

   end Protected_Fifo;

end Util.Concurrent.Fifos;
