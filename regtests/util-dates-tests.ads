-----------------------------------------------------------------------
--  util-dates-tests - Test for dates
--  Copyright (C) 2018, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Util.Dates.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test converting a date in ISO8601.
   procedure Test_ISO8601_Image (T : in out Test);

   --  Test converting a string in ISO8601 into a date.
   procedure Test_ISO8601_Value (T : in out Test);

   --  Test value convertion errors.
   procedure Test_ISO8601_Error (T : in out Test);

   --  Test Is_Same_Day operation.
   procedure Test_Is_Same_Day (T : in out Test);

   --  Test Get_Day_Count operation.
   procedure Test_Get_Day_Count (T : in out Test);

   --  Test the Simple_Format option.
   procedure Test_Simple_Format (T : in out Test);

end Util.Dates.Tests;
