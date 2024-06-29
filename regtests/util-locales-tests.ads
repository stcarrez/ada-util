-----------------------------------------------------------------------
--  util-locales-tests -- Unit tests for Locales
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Locales.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Get_Locale (T : in out Test);
   procedure Test_Hash_Locale (T : in out Test);
   procedure Test_Compare_Locale (T : in out Test);
   procedure Test_Get_Locales (T : in out Test);
   procedure Test_Null_Locale (T : in out Test);

end Util.Locales.Tests;
