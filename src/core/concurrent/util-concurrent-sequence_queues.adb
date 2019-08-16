-----------------------------------------------------------------------
--  util-concurrent-sequence_queues -- Concurrent Fifo Queues
--  Copyright (C) 2019 Stephane Carrez
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
package body Util.Concurrent.Sequence_Queues is

   --  ------------------------------
   --  Put the element in the queue.
   --  ------------------------------
   procedure Enqueue (Into     : in out Queue;
                      Item     : in Element_Type;
                      Sequence : in Sequence_Type;
                      Wait     : in Duration := FOREVER) is
   begin
      if Wait < 0.0 then
         Into.Buffer.Enqueue (Item, Sequence);
      else
         select
            Into.Buffer.Enqueue (Item, Sequence);
         or
            delay Wait;
            raise Timeout;
         end select;
      end if;
   end Enqueue;

   --  ------------------------------
   --  Get the next element in sequence from the queue.
   --  Wait until the element with the last sequence gets available.
   --  The current sequence is then incremented.
   --  ------------------------------
   procedure Dequeue (From     : in out Queue;
                      Item     : out Element_Type;
                      Sequence : out Sequence_Type;
                      Wait     : in Duration := FOREVER) is
   begin
      if Wait < 0.0 then
         From.Buffer.Dequeue (Item, Sequence);
      else
         select
            From.Buffer.Dequeue (Item, Sequence);
         or
            delay Wait;
            raise Timeout;
         end select;
      end if;
   end Dequeue;

   --  ------------------------------
   --  Get the number of elements in the queue.
   --  ------------------------------
   function Get_Count (From : in Queue) return Natural is
   begin
      return From.Buffer.Get_Count;
   end Get_Count;

   --  ------------------------------
   --  Get the sequence number that will be returned by the next Dequeue.
   --  ------------------------------
   function Get_Sequence (From : in Queue) return Sequence_Type is
   begin
      return From.Buffer.Get_Sequence;
   end Get_Sequence;

   --  ------------------------------
   --  Reset the sequence number.
   --  ------------------------------
   procedure Reset_Sequence (Into  : in out Queue;
                             Start : in Sequence_Type := Sequence_Type'First) is
   begin
      Into.Buffer.Set_Sequence (Start);
   end Reset_Sequence;

   --  ------------------------------
   --  Set the queue size.
   --  ------------------------------
   procedure Set_Size (Into : in out Queue;
                       Capacity : in Positive) is
   begin
      Into.Buffer.Set_Size (Capacity);
   end Set_Size;

   --  ------------------------------
   --  Initializes the queue.
   --  ------------------------------
   overriding
   procedure Initialize (Object : in out Queue) is
   begin
      Object.Buffer.Set_Size (Default_Size);
   end Initialize;

   --  ------------------------------
   --  Release the queue elements.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Queue) is
   begin
      Object.Buffer.Set_Size (0);
   end Finalize;

   --  Queue of objects.
   protected body Protected_Queue is

      function Get_Index (Sequence : in Sequence_Type) return Natural is
         Offset : Natural;
      begin
         if Sequence < Seq then
            raise Invalid_Sequence;
         end if;

         Offset := Sequence_Type'Pos (Sequence) - Sequence_Type'Pos (Seq);
         if First + Offset > Elements'Last then
            return Elements'First + Offset - (Elements'Last - First) - 1;
         else
            return First + Offset;
         end if;
      end Get_Index;

      --  ------------------------------
      --  Put the element in the queue.
      --  If the queue is full, wait until some room is available.
      --  ------------------------------
      entry Enqueue (Item     : in Element_Type;
                     Sequence : in Sequence_Type) when Count >= 0 is
         Pos : constant Natural := Get_Index (Sequence);
      begin
         Elements (Pos) := Item;
         States (Pos) := True;
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
      entry Dequeue (Item     : out Element_Type;
                     Sequence : out Sequence_Type) when States (First) is
      begin
         Count := Count - 1;
         Item := Elements (First);
         Sequence := Seq;
         Seq := Sequence_Type'Succ (Seq);

         --  For the clear on dequeue mode, erase the queue element.
         --  If the element holds some storage or a reference, this gets cleared here.
         --  The element used to clear is at index 0 (which does not exist if Clear_On_Dequeue
         --  is false).  There is no overhead when this is not used
         --  (ie, instantiation/compile time flag).
         if Clear_On_Dequeue then
            Elements (First) := Elements (0);
         end if;
         States (First) := False;
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
      --  Get the number of elements in the queue.
      --  ------------------------------
      function Get_Count return Natural is
      begin
         return Count;
      end Get_Count;

      --  ------------------------------
      --  Get the sequence number that will be returned by the next Dequeue.
      --  ------------------------------
      function Get_Sequence return Sequence_Type is
      begin
         return Seq;
      end Get_Sequence;

      --  ------------------------------
      --  Reset the sequence number.
      --  ------------------------------
      procedure Set_Sequence (Start : in Sequence_Type) is
      begin
         Seq := Start;
      end Set_Sequence;

      --  ------------------------------
      --  Set the queue size.
      --  ------------------------------
      procedure Set_Size (Capacity : in Natural) is
         procedure Free is new Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);
         procedure Free is new Ada.Unchecked_Deallocation (Boolean_Array, Boolean_Array_Access);
         First_Pos : Natural := 1;
      begin
         if Clear_On_Dequeue then
            First_Pos := 0;
         end if;
         if Capacity = 0 then
            Free (Elements);
            Free (States);
         elsif Elements = null then
            Elements := new Element_Array (First_Pos .. Capacity);
            States := new Boolean_Array (First_Pos .. Capacity);
            States.all := (others => False);
         else
            declare
               New_Array : constant Element_Array_Access
                 := new Element_Array (First_Pos .. Capacity);
               New_State : constant Boolean_Array_Access
                 := new Boolean_Array (First_Pos .. Capacity);
            begin
               New_State.all := (others => False);
               if Capacity > Elements'Length then
                  New_Array (First_Pos .. Elements'Last) := Elements (First_Pos .. Elements'Last);
                  New_State (First_Pos .. Elements'Last) := States (First_Pos .. Elements'Last);
               else
                  New_Array (First_Pos .. Capacity) := Elements (First_Pos .. Capacity);
                  New_State (First_Pos .. Capacity) := States (First_Pos .. Capacity);
               end if;

               Free (Elements);
               Free (States);
               Elements := New_Array;
               States := New_State;
            end;
         end if;
      end Set_Size;

   end Protected_Queue;

end Util.Concurrent.Sequence_Queues;
