-----------------------------------------------------------------------
--  util-dates-tests - Test for dates
--  Copyright (C) 2018, 2019, 2020 Stephane Carrez
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
with Util.Test_Caller;
with Util.Dates.ISO8601;
package body Util.Dates.Tests is

   package Caller is new Util.Test_Caller (Test, "Dates");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Dates.ISO8601.Image",
                       Test_ISO8601_Image'Access);
      Caller.Add_Test (Suite, "Test Util.Dates.ISO8601.Value",
                       Test_ISO8601_Value'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test converting a date in ISO8601.
   --  ------------------------------
   procedure Test_ISO8601_Image (T : in out Test) is
      T1 : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (1980, 1, 2, 10, 30, 23);
   begin
      Util.Tests.Assert_Equals (T, "1980-01-02", ISO8601.Image (T1),
                                "Invalid Image");

      Util.Tests.Assert_Equals (T, "1980", ISO8601.Image (T1, ISO8601.YEAR),
                                "Invalid Image (YEAR precision)");

      Util.Tests.Assert_Equals (T, "1980-01", ISO8601.Image (T1, ISO8601.MONTH),
                                "Invalid Image (MONTH precision)");

      Util.Tests.Assert_Equals (T, "1980-01-02", ISO8601.Image (T1, ISO8601.DAY),
                                "Invalid Image (DAY precision)");

      Util.Tests.Assert_Equals (T, "1980-01-02T10", ISO8601.Image (T1, ISO8601.HOUR),
                                "Invalid Image (DAY precision)");

      Util.Tests.Assert_Equals (T, "1980-01-02T10:30", ISO8601.Image (T1, ISO8601.MINUTE),
                                "Invalid Image (MINUTE precision)");

      Util.Tests.Assert_Equals (T, "1980-01-02T10:30:23", ISO8601.Image (T1, ISO8601.SECOND),
                                "Invalid Image (SECOND precision)");

   end Test_ISO8601_Image;

   --  ------------------------------
   --  Test converting a string in ISO8601 into a date.
   --  ------------------------------
   procedure Test_ISO8601_Value (T : in out Test) is
      Date   : Ada.Calendar.Time;
      Info   : Date_Record;
   begin
      Date := ISO8601.Value ("1980");
      Util.Tests.Assert_Equals (T, "1980-01-01", ISO8601.Image (Date));

      Date := ISO8601.Value ("1980-03-04");
      Util.Tests.Assert_Equals (T, "1980-03-04", ISO8601.Image (Date));

      Date := ISO8601.Value ("1980-12-31");
      Util.Tests.Assert_Equals (T, "1980-12-31", ISO8601.Image (Date));

      Date := ISO8601.Value ("1980-12-31T11:23");
      Util.Tests.Assert_Equals (T, "1980-12-31T11:23", ISO8601.Image (Date, ISO8601.MINUTE));
   end Test_ISO8601_Value;

end Util.Dates.Tests;
