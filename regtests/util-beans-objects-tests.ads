-----------------------------------------------------------------------
--  util-beans-objects-tests -- Unit tests for objects
--  Copyright (C) 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Beans.Objects.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the creation, initialization and retrieval of objects holding arrays.
   procedure Test_Is_Array (T : in out Test);

   --  Test the Get_Count operation.
   procedure Test_Get_Count (T : in out Test);

   --  Test the Get_Value operation.
   procedure Test_Get_Value (T : in out Test);

   --  Test the Set_Value operation.
   procedure Test_Set_Value (T : in out Test);

   --  Test the "&" operator.
   procedure Test_And_Operator (T : in out Test);

   --  Test the Blob bean.
   procedure Test_Blob (T : in out Test);

end Util.Beans.Objects.Tests;
