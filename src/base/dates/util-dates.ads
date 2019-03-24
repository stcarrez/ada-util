-----------------------------------------------------------------------
--  util-dates -- Date utilities
--  Copyright (C) 2011, 2013, 2018, 2019 Stephane Carrez
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

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Arithmetic;
with Ada.Calendar.Time_Zones;

--  = Date Utilities =
--  The `Util.Dates` package provides various date utilities to help in formatting and parsing
--  dates in various standard formats.  It completes the standard `Ada.Calendar.Formatting` and
--  other packages by implementing specific formatting and parsing. To use the packages
--  described here, use the following GNAT project:
--
--    with "utilada_base";
--
--  == Date Operations ==
--  Several operations allow to compute from a given date:
--
--    * `Get_Day_Start`: The start of the day (0:00),
--    * `Get_Day_End`: The end of the day (23:59:59),
--    * `Get_Week_Start`: The start of the week,
--    * `Get_Week_End`: The end of the week,
--    * `Get_Month_Start`: The start of the month,
--    * `Get_Month_End`: The end of the month
--
--  The `Date_Record` type represents a date in a split format allowing
--  to access easily the day, month, hour and other information.
--
--    Now        : Ada.Calendar.Time := Ada.Calendar.Clock;
--    Week_Start : Ada.Calendar.Time := Get_Week_Start (Now);
--    Week_End   : Ada.Calendar.Time := Get_Week_End (Now);
--
--  @include util-dates-rfc7231.ads
--  @include util-dates-iso8601.ads
--  @include util-dates-formats.ads
package Util.Dates is

   --  The Unix equivalent of 'struct tm'.
   type Date_Record is record
      Date        : Ada.Calendar.Time;
      Year        : Ada.Calendar.Year_Number := 1901;
      Month       : Ada.Calendar.Month_Number := 1;
      Month_Day   : Ada.Calendar.Day_Number := 1;
      Day         : Ada.Calendar.Formatting.Day_Name := Ada.Calendar.Formatting.Tuesday;
      Hour        : Ada.Calendar.Formatting.Hour_Number := 0;
      Minute      : Ada.Calendar.Formatting.Minute_Number := 0;
      Second      : Ada.Calendar.Formatting.Second_Number := 0;
      Sub_Second  : Ada.Calendar.Formatting.Second_Duration := 0.0;
      Time_Zone   : Ada.Calendar.Time_Zones.Time_Offset := 0;
      Leap_Second : Boolean := False;
   end record;

   --  Split the date into a date record (See Ada.Calendar.Formatting.Split).
   procedure Split (Into       : out Date_Record;
                    Date       : in Ada.Calendar.Time;
                    Time_Zone  : in Ada.Calendar.Time_Zones.Time_Offset := 0);

   --  Return the date from the date record (See Ada.Calendar.Formatting.Time_Of).
   function Time_Of (Date : in Date_Record) return Ada.Calendar.Time;

   --  Returns true if the given year is a leap year.
   function Is_Leap_Year (Year : in Ada.Calendar.Year_Number) return Boolean;

   --  Returns true if both dates are on the same day.
   function Is_Same_Day (Date1, Date2 : in Ada.Calendar.Time) return Boolean;
   function Is_Same_Day (Date1, Date2 : in Date_Record) return Boolean;

   --  Get the number of days in the given year.
   function Get_Day_Count (Year : in Ada.Calendar.Year_Number)
                           return Ada.Calendar.Arithmetic.Day_Count;

   --  Get the number of days in the given month.
   function Get_Day_Count (Year  : in Ada.Calendar.Year_Number;
                           Month : in Ada.Calendar.Month_Number)
                           return Ada.Calendar.Arithmetic.Day_Count;

   --  Get a time representing the given date at 00:00:00.
   function Get_Day_Start (Date : in Date_Record) return Ada.Calendar.Time;
   function Get_Day_Start (Date : in Ada.Calendar.Time) return Ada.Calendar.Time;

   --  Get a time representing the given date at 23:59:59.
   function Get_Day_End (Date : in Date_Record) return Ada.Calendar.Time;
   function Get_Day_End (Date : in Ada.Calendar.Time) return Ada.Calendar.Time;

   --  Get a time representing the beginning of the week at 00:00:00.
   function Get_Week_Start (Date : in Date_Record) return Ada.Calendar.Time;
   function Get_Week_Start (Date : in Ada.Calendar.Time) return Ada.Calendar.Time;

   --  Get a time representing the end of the week at 23:59:99.
   function Get_Week_End (Date : in Date_Record) return Ada.Calendar.Time;
   function Get_Week_End (Date : in Ada.Calendar.Time) return Ada.Calendar.Time;

   --  Get a time representing the beginning of the month at 00:00:00.
   function Get_Month_Start (Date : in Date_Record) return Ada.Calendar.Time;
   function Get_Month_Start (Date : in Ada.Calendar.Time) return Ada.Calendar.Time;

   --  Get a time representing the end of the month at 23:59:59.
   function Get_Month_End (Date : in Date_Record) return Ada.Calendar.Time;
   function Get_Month_End (Date : in Ada.Calendar.Time) return Ada.Calendar.Time;

end Util.Dates;
