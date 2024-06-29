-----------------------------------------------------------------------
--  util-http-headers-tests - Unit tests for Headers
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Http.Headers.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the Get_Accepted function.
   procedure Test_Get_Accepted (T : in out Test);

end Util.Http.Headers.Tests;
