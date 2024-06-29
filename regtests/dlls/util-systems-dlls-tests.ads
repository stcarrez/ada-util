-----------------------------------------------------------------------
--  util-systems-dlls-tests -- Unit tests for shared libraries
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Systems.DLLs.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the loading a shared library.
   procedure Test_Load (T : in out Test);

   --  Test getting a shared library symbol.
   procedure Test_Get_Symbol (T : in out Test);

end Util.Systems.DLLs.Tests;
