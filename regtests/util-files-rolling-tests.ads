-----------------------------------------------------------------------
--  util-files-rolling-tests -- Unit tests for rolling file manager
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Files.Rolling.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the Format procedure with its pattern replacement.
   procedure Test_Format (T : in out Test);

   --  Test a rolling manager configured with ascending rolling mode.
   procedure Test_Rolling_Ascending (T : in out Test);

   --  Test a rolling manager configured with descending rolling mode.
   procedure Test_Rolling_Descending (T : in out Test);

end Util.Files.Rolling.Tests;
