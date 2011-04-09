-----------------------------------------------------------------------
--  multipro_refs -- Points out multiprocessor issues with reference counters
--  Copyright (C) 2011 Stephane Carrez
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
with Util.Refs;
procedure Multipro_Refs is

   use Util.Log;
   use Util.Concurrent.Counters;

   Log  : constant Loggers.Logger := Loggers.Create ("multipro");

   type Data is new Util.Refs.Ref_Entity with record
      Value  : Natural;
      Rand   : Natural;
      Result : Long_Long_Integer;
   end record;
   type Data_Access is access all Data;

   package Data_Ref is new Util.Refs.References (Data, Data_Access);

   --  Target counter value we would like.
   Max_Counter       : constant Integer := 1_00_000;

   --  Max number of tasks for executing the concurrent increment.
   Max_Tasks         : constant Integer := 16;

   Unsafe_Ref : Data_Ref.Ref := Data_Ref.Create;
   Safe_Ref   : Data_Ref.Atomic_Ref;

   --  When <b>Run_Safe</b> is false, we use the Ada assignment to update a reference.
   --  The program will crash at a random time due to corruption or multiple free.
   --
   --  When <b>Run_Safe</b> is true, we use the protected type Atomic_Ref to change
   --  the shared reference.  It will not crash.
   Run_Safe    : constant Boolean := True;

   function Get_Reference return Data_Ref.Ref is
   begin
      if Run_Safe then
         return Safe_Ref.Get;
      else
         return Unsafe_Ref;
      end if;
   end Get_Reference;

   procedure Set_Reference (O : in Data_Ref.Ref) is
   begin
      if Run_Safe then
         Safe_Ref.Set (O);
      else
         Unsafe_Ref := O;
      end if;
   end Set_Reference;

   --  Performance measurement.
   Perf    : Util.Measures.Measure_Set;
   T       : Util.Measures.Stamp;
begin
   Safe_Ref.Set (Data_Ref.Create);
   Get_Reference.Value.all.Value := 0;
   for Task_Count in 1 .. Max_Tasks loop
      declare
         --  Each task will increment the counter by the following amount.
         Increment_By_Task : constant Integer := Max_Counter / Task_Count;

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
               end;

               --  Get the data, compute something and change the reference.
               for I in 1 .. Cnt loop
                  declare
                     Ref  : constant Data_Ref.Ref := Get_Reference;
                     Ref2 : constant Data_Ref.Ref := Data_Ref.Create;
                  begin
                     Ref2.Value.all.Value := Ref.Value.all.Value + 1;
                     Ref2.Value.all.Rand  := Cnt;
                     Ref2.Value.all.Result := Long_Long_Integer (Ref2.Value.all.Rand * Cnt)
                       * Long_Long_Integer (Ref2.Value.all.Value);
                     Set_Reference (Ref2);
                     Util.Concurrent.Counters.Increment (Counter);
                  end;
               end loop;
            exception
               when others =>
                  Log.Error ("Exception raised");
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

         Log.Info ("Data.Value  := " & Natural'Image (Get_Reference.Value.all.Value));
         Log.Info ("Data.Rand   := " & Natural'Image (Get_Reference.Value.all.Rand));
         Log.Info ("Data.Result := " & Long_Long_Integer'Image (Get_Reference.Value.all.Result));
      end;
   end loop;

   --  Dump the result
   Util.Measures.Write (Perf, "Multipro", Ada.Text_IO.Standard_Output);
end Multipro_Refs;
