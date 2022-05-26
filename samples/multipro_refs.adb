-----------------------------------------------------------------------
--  multipro_refs -- Points out multiprocessor issues with reference counters
--  Copyright (C) 2011, 2019, 2022 Stephane Carrez
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
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
procedure Multipro_Refs is

   use Util.Log;

   Log  : constant Loggers.Logger := Loggers.Create ("multipro");

   type Data is new Util.Refs.Ref_Entity with record
      Value  : Natural;
      Rand   : Natural;
      Result : Long_Long_Integer;
   end record;
   type Data_Access is access all Data;

   package Data_Ref is new Util.Refs.References (Data, Data_Access);
   package Atomic_Data_Ref is new Data_Ref.IR.Atomic;

   package Hash_Map is new Ada.Containers.Indefinite_Hashed_Maps (String, String,
                                                                  Ada.Strings.Hash,
                                                                  "=");
   type Cache is new Util.Refs.Ref_Entity with record
      Map : Hash_Map.Map;
   end record;
   type Cache_Access is access all Cache;
   package Hash_Ref is new Util.Refs.References (Cache, Cache_Access);
   package Atomic_Hash_Ref is new Hash_Ref.IR.Atomic;

   procedure Set_Reference (O : in Data_Ref.Ref);
   function Exists (Key : in String) return Boolean;
   function Find (Key : in String) return String;
   procedure Add (Key : in String; Value : in String);
   function Get_Reference return Data_Ref.Ref;

   R : Atomic_Hash_Ref.Atomic_Ref;

   function Exists (Key : in String) return Boolean is
      C : constant Hash_Ref.Ref := R.Get;
   begin
      return C.Value.Map.Contains (Key);
   end Exists;

   function Find (Key : in String) return String is
      C : constant Hash_Ref.Ref := R.Get;
   begin
      if C.Value.Map.Contains (Key) then
         return C.Value.Map.Element (Key);
      else
         return "";
      end if;
   end Find;

   procedure Add (Key : in String; Value : in String) is
      C : constant Hash_Ref.Ref := R.Get;
      N : constant Hash_Ref.Ref := Hash_Ref.Create;
   begin
      N.Value.Map := C.Value.Map;
      N.Value.Map.Include (Key, Value);
      R.Set (N);
   end Add;

   --  Target counter value we would like.
   Max_Counter       : constant Integer := 1_00_000;

   --  Max number of tasks for executing the concurrent increment.
   Max_Tasks         : constant Integer := 16;

   Unsafe_Ref : Data_Ref.Ref := Data_Ref.Create;
   Safe_Ref   : Atomic_Data_Ref.Atomic_Ref;

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
   Get_Reference.Value.Value := 0;
   for Task_Count in 1 .. Max_Tasks loop
      R.Set (Hash_Ref.Create);
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
               entry Start (Count : in Natural;
                            Ident : in Integer);
            end Worker;

            task body Worker is
               Cnt : Natural;
               Id  : Integer;
            begin
               accept Start (Count : in Natural;
                             Ident : in Integer) do
                  Cnt := Count;
                  Id  := Ident;
               end Start;

               --  Get the data, compute something and change the reference.
               for I in 1 .. Cnt loop
                  declare
                     Ref  : constant Data_Ref.Ref := Get_Reference;
                     Ref2 : constant Data_Ref.Ref := Data_Ref.Create;
                     Key  : constant String := "K" & Natural'Image (I / 10);
                  begin
                     Ref2.Value.Value := Ref.Value.Value + 1;
                     Ref2.Value.Rand  := Cnt;
                     Ref2.Value.Result := Long_Long_Integer (Ref2.Value.Rand * Cnt)
                       * Long_Long_Integer (Ref2.Value.Value);
                     Set_Reference (Ref2);
                     Util.Concurrent.Counters.Increment (Counter);

                     if not Exists (Key) then
                        Add (Key, Natural'Image (I));
                     end if;
                     declare
                        S : constant String := Find (Key);
                        pragma Unreferenced (S);
                     begin
                        null;
                     exception
                        when others =>
                           Log.Info ("{0}: Find did not found the key: {1}",
                                     Integer'Image (Id), Key);
                     end;
                  end;
               end loop;
            exception
               when E : others =>
                  Log.Error ("Exception raised: ", E, True);
            end Worker;

            type Worker_Array is array (1 .. Task_Count) of Worker;

            Tasks : Worker_Array;

         begin
            Log.Info ("Starting " & Integer'Image (Task_Count) & " tasks");
            for I in Tasks'Range loop
               Tasks (I).Start (Increment_By_Task, I);
            end loop;

            --  Leaving the Worker task scope means we are waiting for our tasks to finish.
         end;

         Util.Measures.Report (Measures => Perf,
                               S        => T,
                               Title    => "Increment counter with "
                               & Integer'Image (Task_Count) & " tasks");

         Log.Info ("Data.Value  := " & Natural'Image (Get_Reference.Value.Value));
         Log.Info ("Data.Rand   := " & Natural'Image (Get_Reference.Value.Rand));
         Log.Info ("Data.Result := " & Long_Long_Integer'Image (Get_Reference.Value.Result));
      end;
   end loop;

   --  Dump the result
   Util.Measures.Write (Perf, "Multipro", Ada.Text_IO.Standard_Output);
end Multipro_Refs;
