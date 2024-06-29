-----------------------------------------------------------------------
--  util-beans-objects-record_tests -- Unit tests for objects.records package
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Beans.Objects.Record_Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Record (T : in out Test);

   procedure Test_Bean (T : in out Test);

end Util.Beans.Objects.Record_Tests;
