-----------------------------------------------------------------------
--  multipro -- Points out multiprocessor issues when incrementing counters
--  Copyright (C) 2010, 2011 Stephane Carrez
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
with Util.Log;
with Util.Log.Loggers;
with Util.Concurrent.Counters;
with Util.Measures;
with Ada.Text_IO;
procedure Multipro is

   use Util.Log;
   use Util.Concurrent.Counters;

   Log  : constant Loggers.Logger := Loggers.Create ("multipro");

   --  Target counter value we would like.
   Max_Counter       : constant Integer := 10_000_000;

   --  Max number of tasks for executing the concurrent increment.
   Max_Tasks         : constant Integer := 16;

   --  Performance measurement.
   Perf    : Util.Measures.Measure_Set;
   T       : Util.Measures.Stamp;
begin
   for Task_Count in 1 .. Max_Tasks loop
      declare
         --  Each task will increment the counter by the following amount.
         Increment_By_Task : constant Integer := Max_Counter / Task_Count;

         --  Counter not protected for concurrency access.
         Unsafe  : Integer := 0;

         --  Counter protected by concurrent accesses.
         Counter : Util.Concurrent.Counters.Counter;
      begin
         declare
            --  A task that increments the shared counter <b>Unsafe</b> and <b>Counter</b> by
            --  the specified amount.
            task type Worker is
               entry Start (Count : in Natural);
            end Worker;

            task body Worker is
               Cnt : Natural;
            begin
               accept Start (Count : in Natural) do
                  Cnt := Count;
               end Start;
               --  Increment the two counters as many times as necessary.
               for I in 1 .. Cnt loop
                  Util.Concurrent.Counters.Increment (Counter);
                  Unsafe := Unsafe + 1;
               end loop;
            end Worker;

            type Worker_Array is array (1 .. Task_Count) of Worker;

            Tasks : Worker_Array;

         begin
            Log.Info ("Starting " & Integer'Image (Task_Count) & " tasks");
            for I in Tasks'Range loop
               Tasks (I).Start (Increment_By_Task);
            end loop;

            --  Leaving the Worker task scope means we are waiting for our tasks to finish.
         end;

         Util.Measures.Report (Measures => Perf,
                               S        => T,
                               Title    => "Increment counter with "
                               & Integer'Image (Task_Count) & " tasks");

         --  Unsafe will be equal to Counter if Task_Count = 1 or if the host is mono-processor.
         --  On dual/quad core, the Unsafe value becomes random and gets lower each time
         --  the number of tasks increases.
         Log.Info ("Expected value at the end      : "
                   & Integer'Image (Increment_By_Task * Task_Count));
         Log.Info ("Counter value at the end       : " & Integer'Image (Value (Counter)));
         Log.Info ("Unprotected counter at the end : " & Integer'Image (Unsafe));
      end;
   end loop;

   --  Dump the result
   Util.Measures.Write (Perf, "Multipro", Ada.Text_IO.Standard_Output);
end Multipro;
