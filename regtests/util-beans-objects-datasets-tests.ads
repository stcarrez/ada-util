-----------------------------------------------------------------------
--  util-beans-objects-datasets-tests -- Unit tests for dataset beans
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Beans.Objects.Datasets.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the creation, initialization and retrieval of dataset content.
   procedure Test_Fill_Dataset (T : in out Test);

end Util.Beans.Objects.Datasets.Tests;
