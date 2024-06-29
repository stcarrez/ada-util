-----------------------------------------------------------------------
--  util-texts-builders_tests -- Unit tests for text builders
--  Copyright (C) 2013, 2016, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
