-----------------------------------------------------------------------
--  util-dates-tests - Test for dates
--  Copyright (C) 2018, 2019, 2020, 2022 Stephane Carrez
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
with Util.Dates.Simple_Format;
package body Util.Dates.Tests is

   package Caller is new Util.Test_Caller (Test, "Dates");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Dates.ISO8601.Image",
                       Test_ISO8601_Image'Access);
      Caller.Add_Test (Suite, "Test Util.Dates.ISO8601.Value",
                       Test_ISO8601_Value'Access);
      Caller.Add_Test (Suite, "Test Util.Dates.ISO8601.Value (Errors)",
                       Test_ISO8601_Error'Access);
      Caller.Add_Test (Suite, "Test Util.Dates.Is_Same_Day",
                       Test_Is_Same_Day'Access);
      Caller.Add_Test (Suite, "Test Util.Dates.Get_Day_Count",
                       Test_Get_Day_Count'Access);
      Caller.Add_Test (Suite, "Test Util.Dates.Simple_Format",
                       Test_Simple_Format'Access);
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
   begin
      Date := ISO8601.Value ("1980");
      Util.Tests.Assert_Equals (T, "1980-01-01", ISO8601.Image (Date));

      Date := ISO8601.Value ("1980-02");
      Util.Tests.Assert_Equals (T, "1980-02", ISO8601.Image (Date, ISO8601.MONTH));

      Date := ISO8601.Value ("1980-03-04");
      Util.Tests.Assert_Equals (T, "1980-03-04", ISO8601.Image (Date));

      Date := ISO8601.Value ("1980-03-04");
      Util.Tests.Assert_Equals (T, "1980-03-04", ISO8601.Image (Date));

      Date := ISO8601.Value ("1980-12-31");
      Util.Tests.Assert_Equals (T, "1980-12-31", ISO8601.Image (Date));

      Date := ISO8601.Value ("19801231");
      Util.Tests.Assert_Equals (T, "1980-12-31", ISO8601.Image (Date));

      Date := ISO8601.Value ("1980-12-31T11:23");
      Util.Tests.Assert_Equals (T, "1980-12-31T11:23", ISO8601.Image (Date, ISO8601.MINUTE));

      Date := ISO8601.Value ("1980-12-31T11:23:34");
      Util.Tests.Assert_Equals (T, "1980-12-31T11:23:34", ISO8601.Image (Date, ISO8601.SECOND));

      Date := ISO8601.Value ("1980-12-31T11:23:34.123");
      Util.Tests.Assert_Equals (T, "1980-12-31T11:23:34.123+00:00",
                                ISO8601.Image (Date, ISO8601.SUBSECOND));

      Date := ISO8601.Value ("1980-12-31T11:23:34.123+04:30");
      --  The date was normalized in GMT
      Util.Tests.Assert_Equals (T, "1980-12-31T06:53:34.123+00:00",
                                ISO8601.Image (Date, ISO8601.SUBSECOND));
   end Test_ISO8601_Value;

   --  ------------------------------
   --  Test value convertion errors.
   --  ------------------------------
   procedure Test_ISO8601_Error (T : in out Test) is
      procedure Check (Date : in String);

      procedure Check (Date : in String) is
      begin
         declare
            Unused : constant Ada.Calendar.Time := ISO8601.Value (Date);
         begin
            T.Fail ("No exception raised for " & Date);

         end;

      exception
         when Constraint_Error =>
            null;
      end Check;

   begin
      Check ("");
      Check ("1980-");
      Check ("1980:02:03");
      Check ("1980-02-03u33:33");
      Check ("1980-02-03u33");
      Check ("1980-13-44");
      Check ("1980-12-00");
      Check ("1980-12-03T25:34");
      Check ("1980-12-03T10x34");
      Check ("1980-12-03T10:34p");
      Check ("1980-12-31T11:23:34123");
      Check ("1980-12-31T11:23:34,1");
      Check ("1980-12-31T11:23:34,12");
      Check ("1980-12-31T11:23:34x123");
      Check ("1980-12-31T11:23:34.1234");
      Check ("1980-12-31T11:23:34Zp");
      Check ("1980-12-31T11:23:34+2");
      Check ("1980-12-31T11:23:34+23x");
      Check ("1980-12-31T11:23:34+99");
      Check ("1980-12-31T11:23:34+10:0");
      Check ("1980-12-31T11:23:34+10:03x");
   end Test_ISO8601_Error;

   --  ------------------------------
   --  Test Is_Same_Day operation.
   --  ------------------------------
   procedure Test_Is_Same_Day (T : in out Test) is
      procedure Check (D1, D2   : in String;
                       Same_Day : in Boolean);

      procedure Check (D1, D2   : in String;
                       Same_Day : in Boolean) is
         T1 : constant Ada.Calendar.Time := ISO8601.Value (D1);
         T2 : constant Ada.Calendar.Time := ISO8601.Value (D2);
      begin
         T.Assert (Same_Day = Is_Same_Day (T1, T2), "Invalid Is_Same_Day for " & D1 & " " & D2);
         T.Assert (Same_Day = Is_Same_Day (T2, T1), "Invalid Is_Same_Day for " & D2 & " " & D1);
      end Check;

   begin
      Check ("1980-12-31T11:23:34.123", "1980-12-31T10:23:34.123", True);
      Check ("1980-12-31T11:23:34.123", "1980-12-30T10:23:34.123", False);
      Check ("1980-12-31T00:00:00", "1980-12-31T23:59:59", True);
   end Test_Is_Same_Day;

   --  ------------------------------
   --  Test Get_Day_Count operation.
   --  ------------------------------
   procedure Test_Get_Day_Count (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, 366, Natural (Get_Day_Count (2020)));
      Util.Tests.Assert_Equals (T, 365, Natural (Get_Day_Count (1983)));
      Util.Tests.Assert_Equals (T, 366, Natural (Get_Day_Count (2000)));
      Util.Tests.Assert_Equals (T, 365, Natural (Get_Day_Count (2001)));
   end Test_Get_Day_Count;

   --  ------------------------------
   --  Test the Simple_Format option.
   --  ------------------------------
   procedure Test_Simple_Format (T : in out Test) is
      procedure Check (Pattern, Expect : in String);

      Date : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (2021, 02, 12,
                                                                            13, 31, 4, 0.123456);
      procedure Check (Pattern, Expect : in String) is
      begin
         Util.Tests.Assert_Equals (T, Expect, Simple_Format (Pattern, Date), "Simple_Format");
      end Check;

   begin
      Check ("YYYY", "2021");
      Check ("YYY", "YYY");
      Check ("Year: YYYY Month: MM Day: dd", "Year: 2021 Month: 02 Day: 12");
      Check ("HH:mm:ss", "13:31:04");
   end Test_Simple_Format;

end Util.Dates.Tests;
