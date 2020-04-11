-----------------------------------------------------------------------
--  util-concurrent-fifos -- Concurrent Fifo Queues
--  Copyright (C) 2012, 2017, 2020 Stephane Carrez
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

--  The `Util.Concurrent.Fifos` generic defines a queue of objects which
--  can be shared by multiple threads.  First, the queue size is configured
--  by using the `Set_Size` procedure.  Then, a thread can insert elements
--  in the queue by using the `Enqueue` procedure.  The thread will block
--  if the queue is full.  Another thread can use the `Dequeue` procedure
--  to fetch the oldest element from the queue.  The thread will block
--  until an element is inserted if the queue is empty.
generic
   type Element_Type is private;

   --  The default queue size.
   Default_Size     : in Positive;

   --  After a dequeue, clear the element stored in the queue.
   Clear_On_Dequeue : in Boolean := False;
package Util.Concurrent.Fifos is

   FOREVER : constant Duration := -1.0;

   --  Exception raised if the enqueue or dequeue timeout exceeded.
   Timeout : exception;

   --  Fifo of objects
   type Fifo is new Ada.Finalization.Limited_Controlled with private;

   --  Put the element in the queue.
   procedure Enqueue (Into    : in out Fifo;
                      Item    : in Element_Type;
                      Wait    : in Duration := FOREVER);

   --  Get an element from the queue.
   --  Wait until one element gets available.
   procedure Dequeue (From    : in out Fifo;
                      Item    : out Element_Type;
                      Wait    : in Duration := FOREVER);

   --  Wait for the fifo to become empty.
   procedure Wait_Empty (From : in out Fifo);

   --  Get the number of elements in the queue.
   function Get_Count (From : in Fifo) return Natural;

   --  Set the queue size.
   procedure Set_Size (Into : in out Fifo;
                       Capacity : in Positive);

   --  Initializes the queue.
   overriding
   procedure Initialize (Object : in out Fifo);

   --  Release the queue elements.
   overriding
   procedure Finalize (Object : in out Fifo);

private

   --  To store the queue elements, we use an array which is allocated dynamically
   --  by the `Set_Size` protected operation.  The generated code is smaller
   --  compared to the use of Ada vectors container.
   type Element_Array is array (Natural range <>) of Element_Type;
   type Element_Array_Access is access all Element_Array;

   Null_Element_Array : constant Element_Array_Access := null;

   --  Queue of objects.
   protected type Protected_Fifo is

      --  Put the element in the queue.
      --  If the queue is full, wait until some room is available.
      entry Enqueue (Item : in Element_Type);

      --  Get an element from the queue.
      --  Wait until one element gets available.
      entry Dequeue (Item : out Element_Type);

      --  Wait for the queue to become empty.
      entry Wait_Empty;

      --  Get the number of elements in the queue.
      function Get_Count return Natural;

      --  Set the queue size.
      procedure Set_Size (Capacity : in Natural);

   private
      First     : Positive := 1;
      Last      : Positive := 1;
      Count     : Natural := 0;
      Elements  : Element_Array_Access := Null_Element_Array;
   end Protected_Fifo;

   type Fifo is new Ada.Finalization.Limited_Controlled with record
      Buffer : Protected_Fifo;
   end record;

end Util.Concurrent.Fifos;
