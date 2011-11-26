-----------------------------------------------------------------------
--  concurrency.tests -- Unit tests for concurrency package
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Finalization;

with Util.Tests;
with Util.Concurrent.Counters;
with Util.Test_Caller;
with Util.Measures;
with Util.Concurrent.Copies;
with Util.Concurrent.Pools;
package body Util.Concurrent.Tests is

   use Util.Tests;
   use Util.Concurrent.Counters;

   type Connection is new Ada.Finalization.Controlled with record
      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Value : Natural := 0;
   end record;

   overriding
   procedure Finalize (C : in out Connection);

   package Connection_Pool is new Util.Concurrent.Pools (Connection);

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Concurrent.Counter.Increment",
                       Test_Increment'Access);
      Caller.Add_Test (Suite, "Test Util.Concurrent.Counter.Decrement",
                       Test_Decrement'Access);
      Caller.Add_Test (Suite, "Test Util.Concurrent.Counter.Decrement + Test",
                       Test_Decrement_And_Test'Access);
      Caller.Add_Test (Suite, "Test Util.Concurrent.Copies.Get + Set",
                       Test_Copy'Access);
      Caller.Add_Test (Suite, "Test Util.Concurrent.Pools.Get_Instance",
                       Test_Pool'Access);
      Caller.Add_Test (Suite, "Test Util.Concurrent.Pools (concurrent test)",
                       Test_Concurrent_Pool'Access);
   end Add_Tests;

   overriding
   procedure Finalize (C : in out Connection) is
   begin
      null;
   end Finalize;

   --  ------------------------------
   --  Test concurrent pool
   --  ------------------------------
   procedure Test_Pool (T : in out Test) is
      use Ada.Strings.Unbounded;

      P : Connection_Pool.Pool;
      C : Connection;
   begin
      --  Set the pool capacity.
      P.Set_Size (Capacity => 10);

      --  Insert the objects.
      for I in 1 .. 10 loop
         C.Name := To_Unbounded_String (Integer'Image (I));
         P.Release (C);
      end loop;

      --  Use the pool and verify the objects.
      declare
         C : array (1 .. 10) of Connection;
      begin
         for J in 1 .. 3 loop
            --  Get each object and verify that it matches our instance.
            for I in reverse 1 .. 10 loop
               P.Get_Instance (C (I));
               Assert_Equals (T, Integer'Image (I), To_String (C (I).Name), "Invalid pool object");
            end loop;

            --  Put the object back in the pool.
            for I in 1 .. 10 loop
               P.Release (C (I));
            end loop;
         end loop;
      end;

      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            P.Get_Instance (C);
            P.Release (C);
         end loop;
         Util.Measures.Report (S, "1000 Pool Get_Instance+Release");
      end;
   end Test_Pool;

   --  ------------------------------
   --  Test concurrent pool
   --  ------------------------------
   procedure Test_Concurrent_Pool (T : in out Test) is
      use Ada.Strings.Unbounded;

      Count_By_Task : constant Natural := 100_001;
      Task_Count    : constant Natural := 17;
      Capacity      : constant Natural := 5;

      P : Connection_Pool.Pool;
      C : Connection;
      S : Util.Measures.Stamp;
   begin
      --  Set the pool capacity.
      P.Set_Size (Capacity => Capacity);

      --  Insert the objects.
      for I in 1 .. Capacity loop
         C.Name := To_Unbounded_String (Integer'Image (I));
         P.Release (C);
      end loop;

      declare

         --  A task that picks an object from the pool, increment the value and puts
         --  back the object in the pool.
         task type Worker is
            entry Start (Count : in Natural);
         end Worker;

         task body Worker is
            Cnt : Natural;
         begin
            accept Start (Count : in Natural) do
               Cnt := Count;
            end Start;
            --  Get an object from the pool, increment the value and put it back in the pool.
            for I in 1 .. Cnt loop
               declare
                  C : Connection;
               begin
                  P.Get_Instance (C);
                  C.Value := C.Value + 1;
                  P.Release (C);
               end;
            end loop;
         exception
            when others =>
               Ada.Text_IO.Put_Line ("Exception raised.");

         end Worker;

         type Worker_Array is array (1 .. Task_Count) of Worker;

         Tasks : Worker_Array;
      begin
         for I in Tasks'Range loop
            Tasks (I).Start (Count_By_Task);
         end loop;

         --  Leaving the Worker task scope means we are waiting for our tasks to finish.
      end;
      Util.Measures.Report (S, "Executed Get+Release " & Natural'Image (Count_By_Task * Task_Count));
      declare
         Total : Natural := 0;
      begin
         for I in 1 .. Capacity loop
            P.Get_Instance (C);
            Total := Total + C.Value;
         end loop;
         Assert_Equals (T, Count_By_Task * Task_Count, Total, "Invalid computation");
      end;
   end Test_Concurrent_Pool;

   procedure Test_Increment (T : in out Test) is
      C : Counter;
   begin
      Increment (C);
      Assert_Equals (T, Value (C), 1, "Increment failed");
   end Test_Increment;

   procedure Test_Decrement (T : in out Test) is
      C : Counter;
   begin
      Increment (C);
      Decrement (C);
      Assert_Equals (T, Value (C), 0, "Increment + Decrement failed");
   end Test_Decrement;

   procedure Test_Decrement_And_Test (T : in out Test) is
      C : Counter;
      Is_Zero : Boolean;
   begin
      Increment (C);
      Assert_Equals (T, Value (C), 1, "Increment failed");
      Decrement (C, Is_Zero);
      Assert_Equals (T, Value (C), 0, "Decrement failed");
      T.Assert (Is_Zero, "Counter should be zero");
      Increment (C);
      Increment (C);
      Decrement (C, Is_Zero);
      T.Assert (not Is_Zero, "Counter should not be zero");
   end Test_Decrement_And_Test;

   procedure Test_Copy (T : in out Test) is

      type Data is record
         C : Natural := 0;
         L : Long_Long_Integer := 1;
         B : Boolean := False;
         F : Float := 1.0;
         S : String (1 .. 10) := (others => ' ');
      end record;

      package Data_Atomic is new Util.Concurrent.Copies (Data);

      D  : Data_Atomic.Atomic;
      V  : Data;
      V2 : Data;
   begin
      V.C := 1;
      V.B := True;
      V.S := "0123456789";
      D.Set (V);

      V2 := D.Get;
      Assert_Equals (T, 1, V2.C, "Invalid Data.C");
      Assert_Equals (T, "0123456789", V2.S, "Invalid Data.S");

      --  Concurrent test:
      --   o increment the long long integer
      --   o rotate the string by 1 position
      declare
         Count_By_Task : constant Natural := 100_001;
         Task_Count    : constant Natural := 17;

         --  A task that increments the shared counter <b>Unsafe</b> and <b>Counter</b> by
         --  the specified amount.
         task type Worker is
            entry Start (Count : in Natural);
         end Worker;

         task body Worker is
            Cnt : Natural;
            Val : Data;
            C   : Character;
         begin
            accept Start (Count : in Natural) do
               Cnt := Count;
            end Start;
            --  Increment the two counters as many times as necessary.
            for I in 1 .. Cnt loop
               Val := D.Get;
--                 Val := V;
               Val.L := Val.L + 1;
               C := Val.S (1);
               Val.S (1 .. 9) := Val.S (2 .. 10);
               Val.S (10) := C;
               D.Set (Val);
               --                 V := Val;
               Val.S (1 .. 9) := Val.S (2 .. 10);
               Val.S (10) := C;
            end loop;
         exception
            when others =>
               Ada.Text_IO.Put_Line ("Exception raised.");

         end Worker;

         type Worker_Array is array (1 .. Task_Count) of Worker;

         Tasks : Worker_Array;
      begin
         for I in Tasks'Range loop
            Tasks (I).Start (Count_By_Task);
         end loop;

         --  Leaving the Worker task scope means we are waiting for our tasks to finish.
      end;

      --  We can't predict the exact value for string after the rotation passes.
      --  At least, we must have one of the following values (when using an unprotected
      --  copy, the string value contains garbage).
      T.Assert (D.Get.S = "0123456789" or D.Get.S = "1234567890" or
                D.Get.S = "2345678901" or D.Get.S = "3456789012" or
                  D.Get.S = "4567890123" or D.Get.S = "5678901234" or
                    D.Get.S = "6789012345" or D.Get.S = "7890123456" or
                      D.Get.S = "8901234567" or D.Get.S = "9012345678",
              "Invalid result: " & D.Get.S);
   end Test_Copy;

end Util.Concurrent.Tests;
