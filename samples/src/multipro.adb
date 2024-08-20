-----------------------------------------------------------------------
--  multipro -- Points out multiprocessor issues when incrementing counters
--  Copyright (C) 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Log;
with Util.Log.Loggers;
with Util.Concurrent.Counters;
with Util.Measures;
with Ada.Text_IO;
procedure Multipro is

   use Util.Log;

   Log  : constant Loggers.Logger := Loggers.Create ("multipro");

   --  Target counter value we would like.
   Max_Counter       : constant Integer := 2000_000_000;

   --  Max number of tasks for executing the concurrent increment.
   Max_Tasks         : constant Integer := 8;

   type Ad is record
      S : String (1 .. 32);
      C : Util.Concurrent.Counters.Counter;
   end record;
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
         --  Counter : Util.Concurrent.Counters.Counter;
      begin
         declare
            --  A task that increments the shared counter <b>Unsafe</b> and <b>Counter</b> by
            --  the specified amount.
            task type Worker is
               entry Start (Id : in Natural; Count : in Natural);
            end Worker;

            task body Worker is
               Cnt : Natural;
               Cid : Natural;
            begin
               accept Start (Id : in Natural; Count : in Natural) do
                  Cid := Id;
                  Cnt := Count;
               end Start;
               --  Increment the two counters as many times as necessary.
               for I in 1 .. Cnt loop
                  --  Util.Concurrent.Counters.Increment (C (Cid).C);
                  Unsafe := Unsafe + 1;
               end loop;
            end Worker;

            type Worker_Array is array (1 .. Task_Count) of Worker;

            Tasks : Worker_Array;

         begin
            Log.Info ("Starting " & Integer'Image (Task_Count) & " tasks");
            for I in Tasks'Range loop
               Tasks (I).Start (I, Increment_By_Task);
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
         --  Log.Info ("Counter value at the end       : " & Integer'Image (Value (Counter)));
         Log.Info ("Unprotected counter at the end : " & Integer'Image (Unsafe));
      end;
   end loop;

   --  Dump the result
   Util.Measures.Write (Perf, "Multipro", Ada.Text_IO.Standard_Output);
end Multipro;
