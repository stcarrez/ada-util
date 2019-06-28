-----------------------------------------------------------------------
--  concurrency.tests -- Unit tests for concurrency package
--  Copyright (C) 2009, 2010, 2011, 2012, 2019 Stephane Carrez
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

package Util.Concurrent.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Increment (T : in out Test);
   procedure Test_Decrement (T : in out Test);
   procedure Test_Decrement_And_Test (T : in out Test);

   procedure Test_Copy (T : in out Test);

   --  Test concurrent pool
   procedure Test_Pool (T : in out Test);

   --  Test concurrent pool
   procedure Test_Concurrent_Pool (T : in out Test);

   --  Test fifo.
   procedure Test_Fifo (T : in out Test);

   --  Test concurrent aspects of fifo.
   procedure Test_Concurrent_Fifo (T : in out Test);

   --  Test concurrent arrays.
   procedure Test_Array (T : in out Test);

   --  Test concurrent sequences.
   procedure Test_Concurrent_Sequences (T : in out Test);

end Util.Concurrent.Tests;
