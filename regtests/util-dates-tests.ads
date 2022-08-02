-----------------------------------------------------------------------
--  util-dates-tests - Test for dates
--  Copyright (C) 2018, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
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
