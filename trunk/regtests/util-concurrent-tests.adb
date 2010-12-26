-----------------------------------------------------------------------
--  concurrency.tests -- Unit tests for concurrency package
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with Util.Tests;
with Util.Concurrent.Counters;
with Util.Test_Caller;
package body Util.Concurrent.Tests is

   use Util.Tests;
   use Util.Concurrent.Counters;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Concurrent.Counter.Increment",
                       Test_Increment'Access);
      Caller.Add_Test (Suite, "Test Util.Concurrent.Counter.Decrement",
                       Test_Decrement'Access);
      Caller.Add_Test (Suite, "Test Util.Concurrent.Counter.Decrement + Test",
                       Test_Decrement_And_Test'Access);
   end Add_Tests;

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

end Util.Concurrent.Tests;
