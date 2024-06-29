-----------------------------------------------------------------------
--  util-properties-tests -- Tests for properties
--  Copyright (C) 2009, 2010, 2011, 2014, 2017, 2020, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Properties.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Property (T : in out Test);
   procedure Test_Integer_Property (T : in out Test);
   procedure Test_Load_Property (T : in out Test);
   procedure Test_Load_Strip_Property (T : in out Test);
   procedure Test_Copy_Property (T : in out Test);
   procedure Test_Set_Preserve_Original (T : in out Test);
   procedure Test_Remove_Preserve_Original (T : in out Test);
   procedure Test_Missing_Property (T : in out Test);
   procedure Test_Load_Ini_Property (T : in out Test);
   procedure Test_Save_Properties (T : in out Test);
   procedure Test_Remove_Property (T : in out Test);

end Util.Properties.Tests;
