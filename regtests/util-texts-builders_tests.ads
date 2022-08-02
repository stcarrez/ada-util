-----------------------------------------------------------------------
--  util-texts-builders_tests -- Unit tests for text builders
--  Copyright (C) 2013, 2016, 2022 Stephane Carrez
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

package Util.Texts.Builders_Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the length operation.
   procedure Test_Length (T : in out Test);

   --  Test the append operation.
   procedure Test_Append (T : in out Test);

   --  Test the append operation.
   procedure Test_Inline_Append (T : in out Test);

   --  Test the iterate operation.
   procedure Test_Iterate (T : in out Test);

   --  Test the iterate operation.
   procedure Test_Inline_Iterate (T : in out Test);

   --  Test the Find generic operation.
   procedure Test_Find (T : in out Test);

   --  Test the clear operation.
   procedure Test_Clear (T : in out Test);

   --  Test the tail operation.
   procedure Test_Tail (T : in out Test);

   --  Test the append and iterate performance.
   procedure Test_Perf (T : in out Test);

   --  Test the Element function.
   procedure Test_Element (T : in out Test);

end Util.Texts.Builders_Tests;
