-----------------------------------------------------------------------
--  util-concurrent-sequence_queues -- Concurrent Sequence Queues
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

with Ada.Finalization;

--  == Sequence Queues ==
--  The sequence queue associates a sequence number to each element inserted in
--  the queue.  Elements can be inserted in any sequence order but they are
--  always dequeued in the increasing sequence order.
generic
   type Element_Type is private;

   type Sequence_Type is (<>);

   --  The default queue size.
   Default_Size     : in Positive;

   --  After a dequeue, clear the element stored in the queue.
   Clear_On_Dequeue : in Boolean := False;
package Util.Concurrent.Sequence_Queues is

   FOREVER : constant Duration := -1.0;

   --  Exception raised if the enqueue or dequeue timeout exceeded.
   Timeout : exception;

   --  Exception raised if the sequence number is out of bounds.
   Invalid_Sequence : exception;

   --  Fifo of objects
   type Queue is new Ada.Finalization.Limited_Controlled with private;

   --  Put the element with the sequence in the queue.
   procedure Enqueue (Into     : in out Queue;
                      Item     : in Element_Type;
                      Sequence : in Sequence_Type;
                      Wait     : in Duration := FOREVER);

   --  Get the next element in sequence from the queue.
   --  Wait until the element with the last sequence gets available.
   --  The current sequence is then incremented.
   procedure Dequeue (From     : in out Queue;
                      Item     : out Element_Type;
                      Sequence : out Sequence_Type;
                      Wait     : in Duration := FOREVER);

   --  Get the number of elements in the queue.
   function Get_Count (From : in Queue) return Natural;

   --  Get the sequence number that will be returned by the next Dequeue.
   function Get_Sequence (From : in Queue) return Sequence_Type;

   --  Reset the sequence number.
   procedure Reset_Sequence (Into  : in out Queue;
                             Start : in Sequence_Type := Sequence_Type'First);

   --  Set the queue size.
   procedure Set_Size (Into : in out Queue;
                       Capacity : in Positive);

   --  Initializes the queue.
   overriding
   procedure Initialize (Object : in out Queue);

   --  Release the queue elements.
   overriding
   procedure Finalize (Object : in out Queue);

private

   --  To store the queue elements, we use an array which is allocated dynamically
   --  by the <b>Set_Size</b> protected operation.  The generated code is smaller
   --  compared to the use of Ada vectors container.
   type Element_Array is array (Natural range <>) of Element_Type;
   type Element_Array_Access is access all Element_Array;

   type Boolean_Array is array (Natural range <>) of Boolean;
   type Boolean_Array_Access is access all Boolean_Array;

   Null_Element_Array : constant Element_Array_Access := null;
   Null_Boolean_Array : constant Boolean_Array_Access := null;

   --  Queue of objects.
   protected type Protected_Queue is

      --  Put the element in the queue.
      --  If the queue is full, wait until some room is available.
      entry Enqueue (Item     : in Element_Type;
                     Sequence : in Sequence_Type);

      --  Get an element from the queue.
      --  Wait until one element gets available.
      entry Dequeue (Item     : out Element_Type;
                     Sequence : out Sequence_Type);

      --  Get the number of elements in the queue.
      function Get_Count return Natural;

      --  Get the sequence number that will be returned by the next Dequeue.
      function Get_Sequence return Sequence_Type;

      --  Reset the sequence number.
      procedure Set_Sequence (Start : in Sequence_Type);

      --  Set the queue size.
      procedure Set_Size (Capacity : in Natural);

   private
      First     : Positive := 1;
      Last      : Positive := 1;
      Seq       : Sequence_Type := Sequence_Type'First;
      Last_Seq  : Sequence_Type := Sequence_Type'Last;
      Count     : Natural := 0;
      Elements  : Element_Array_Access := Null_Element_Array;
      States    : Boolean_Array_Access := Null_Boolean_Array;
   end Protected_Queue;

   type Queue is new Ada.Finalization.Limited_Controlled with record
      Buffer : Protected_Queue;
   end record;

end Util.Concurrent.Sequence_Queues;
