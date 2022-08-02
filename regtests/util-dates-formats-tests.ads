-----------------------------------------------------------------------
--  util-dates-formats-tests - Test for date formats
--  Copyright (C) 2011, 2013, 2014, 2015, 2016, 2018, 2020, 2022 Stephane Carrez
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
package Util.Dates.Formats.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Format (T : in out Test);

   --  Test parsing a date using several formats and different locales.
   procedure Test_Parse (T : in out Test);

   --  Test parsing a date using several formats and having several errors.
   procedure Test_Parse_Error (T : in out Test);

   --  Test the Get_Day_Start operation.
   procedure Test_Get_Day_Start (T : in out Test);

   --  Test the Get_Week_Start operation.
   procedure Test_Get_Week_Start (T : in out Test);

   --  Test the Get_Month_Start operation.
   procedure Test_Get_Month_Start (T : in out Test);

   --  Test the Get_Day_End operation.
   procedure Test_Get_Day_End (T : in out Test);

   --  Test the Get_Week_End operation.
   procedure Test_Get_Week_End (T : in out Test);

   --  Test the Get_Month_End operation.
   procedure Test_Get_Month_End (T : in out Test);

   --  Test the Split operation.
   procedure Test_Split (T : in out Test);

   --  Test the Append_Date operation
   procedure Test_Append_Date (T : in out Test);

   --  Test the ISO8601 operations.
   procedure Test_ISO8601 (T : in out Test);

end Util.Dates.Formats.Tests;
