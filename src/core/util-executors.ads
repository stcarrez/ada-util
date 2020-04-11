-----------------------------------------------------------------------
--  util-executors -- Execute work that is queued
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
with Ada.Exceptions;
private with Util.Concurrent.Fifos;

--  == Executors ==
--  The `Util.Executors` generic package defines a queue of work that will be executed
--  by one or several tasks.  The `Work_Type` describes the type of the work and the
--  `Execute` procedure will be called by the task to execute the work.  After instantiation
--  of the package, an instance of the `Executor_Manager` is created with a number of desired
--  tasks.  The tasks are then started by calling the `Start` procedure.
--
--  A work object is added to the executor's queue by using the `Execute` procedure.
--  The work object is added in a concurrent fifo queue.  One of the task managed by the
--  executor manager will pick the work object and run it.
--
generic
   type Work_Type is private;
   with procedure Execute (Work : in out Work_Type);
   with procedure Error (Work : in out Work_Type;
                         Err  : in Ada.Exceptions.Exception_Occurrence) is null;
   Default_Queue_Size : Positive := 32;
package Util.Executors is

   use Ada.Finalization;

   type Executor_Manager (Count : Positive) is limited new Limited_Controlled with private;

   type Executor_Manager_Access is access all Executor_Manager'Class;

   overriding
   procedure Initialize (Manager : in out Executor_Manager);

   --  Execute the work through the executor.
   procedure Execute (Manager : in out Executor_Manager;
                      Work    : in Work_Type);

   --  Start the executor tasks.
   procedure Start (Manager  : in out Executor_Manager;
                    Autostop : in Boolean := False);

   --  Stop the tasks and wait for their completion.
   procedure Stop (Manager : in out Executor_Manager);

   --  Set the work queue size.
   procedure Set_Queue_Size (Manager  : in out Executor_Manager;
                             Capacity : in Positive);

   --  Wait for the pending work to be executed by the executor tasks.
   procedure Wait (Manager : in out Executor_Manager);

   --  Get the number of elements in the queue.
   function Get_Count (Manager : in Executor_Manager) return Natural;

   --  Stop and release the executor.
   overriding
   procedure Finalize (Manager : in out Executor_Manager);

private

   type Work_Info is record
      Work : Work_Type;
      Done : Boolean := False;
   end record;

   package Work_Queue is
     new Util.Concurrent.Fifos (Element_Type     => Work_Info,
                                Default_Size     => Default_Queue_Size,
                                Clear_On_Dequeue => True);

   task type Worker_Task is
      entry Start (Manager : in Executor_Manager_Access);
   end Worker_Task;

   type Worker_Task_Array is array (Positive range <>) of Worker_Task;

   type Executor_Manager (Count : Positive) is limited new Limited_Controlled with record
      Self     : Executor_Manager_Access;
      Autostop : Boolean := False;
      Queue    : Work_Queue.Fifo;
      Workers  : Worker_Task_Array (1 .. Count);
   end record;

end Util.Executors;
