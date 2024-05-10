-----------------------------------------------------------------------
--  util-executors -- Execute work that is queued
--  Copyright (C) 2019, 2020, 2024 Stephane Carrez
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

package body Util.Executors is

   overriding
   procedure Initialize (Manager : in out Executor_Manager) is
   begin
      Manager.Self := Manager'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   --  Execute the work through the executor.
   --  ------------------------------
   procedure Execute (Manager : in out Executor_Manager;
                      Work    : in Work_Type) is
      W : constant Work_Info := Work_Info '(Work => Work, Done => False);
   begin
      Manager.Queue.Enqueue (W);
   end Execute;

   --  ------------------------------
   --  Start the executor tasks.
   --  ------------------------------
   procedure Start (Manager  : in out Executor_Manager;
                    Autostop : in Boolean := False) is
   begin
      Manager.Autostop := Autostop;
      for Worker of Manager.Workers loop
         Worker.Start (Manager.Self);
      end loop;
   end Start;

   --  ------------------------------
   --  Stop the tasks and wait for their completion.
   --  ------------------------------
   procedure Stop (Manager : in out Executor_Manager) is
      W : Work_Info;
      N : Natural := 0;
   begin
      W.Done := True;
      --  We must process in two separate steps because as soon as we enqueue
      --  a stop work, a next worker in the list could dequeue and stop immediately
      --  and we may loose a correct number of stop work.
      --  Step 1: See how many workers are still running.
      for Worker of Manager.Workers loop
         if not Worker'Terminated then
            N := N + 1;
         end if;
      end loop;

      --  Step 2: Enqueue a stop work for each worker.
      while N > 0 loop
         Manager.Queue.Enqueue (W);
         N := N - 1;
      end loop;
   end Stop;

   --  ------------------------------
   --  Set the work queue size.
   --  ------------------------------
   procedure Set_Queue_Size (Manager  : in out Executor_Manager;
                             Capacity : in Positive) is
   begin
      Manager.Queue.Set_Size (Capacity);
   end Set_Queue_Size;

   --  ------------------------------
   --  Wait for the pending work to be executed by the executor tasks.
   --  ------------------------------
   procedure Wait (Manager : in out Executor_Manager) is
   begin
      Manager.Queue.Wait_Empty;
   end Wait;

   --  ------------------------------
   --  Get the number of elements in the queue.
   --  ------------------------------
   function Get_Count (Manager : in Executor_Manager) return Natural is
   begin
      return Manager.Queue.Get_Count;
   end Get_Count;

   --  ------------------------------
   --  Stop and release the executor.
   --  ------------------------------
   overriding
   procedure Finalize (Manager : in out Executor_Manager) is
   begin
      Manager.Stop;
   end Finalize;

   task body Worker_Task is
      M        : access Executor_Manager;
      Autostop : Boolean := False;
   begin
      select
         accept Start (Manager : in Executor_Manager_Access) do
            M := Manager;
         end Start;
      or
         terminate;
      end select;
      while M /= null loop
         declare
            Work : Work_Info;
         begin
            if Autostop then
               M.Queue.Dequeue (Work, 0.0);
            else
               M.Queue.Dequeue (Work);
            end if;
            exit when Work.Done;

            begin
               Execute (Work.Work);
            exception
               when E : others =>
                  Error (Work.Work, E);
            end;
            Autostop := M.Autostop;

         exception
            when Work_Queue.Timeout =>
               exit;
         end;
      end loop;
   end Worker_Task;

end Util.Executors;
