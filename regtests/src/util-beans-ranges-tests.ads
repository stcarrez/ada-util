-----------------------------------------------------------------------
--  util-beans-ranges-tests -- Unit tests for bean range definitions
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Beans.Ranges.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the creation and range definition.
   procedure Test_Range (T : in out Test);

   --  Test iterating over a range definition.
   procedure Test_Iterate_Range (T : in out Test);

end Util.Beans.Ranges.Tests;
