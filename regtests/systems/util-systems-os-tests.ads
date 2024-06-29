-----------------------------------------------------------------------
--  util-systems-os-tests -- Unit tests for OS specific operations
--  Copyright (C) 2014, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Systems.Os.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the Sys_Stat operation.
   procedure Test_Stat (T : in out Test);

   --  Test the Sys_Stat operation on a directory.
   procedure Test_Stat_Directory (T : in out Test);

end Util.Systems.Os.Tests;
