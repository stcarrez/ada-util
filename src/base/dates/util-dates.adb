-----------------------------------------------------------------------
--  util-dates -- Date utilities
--  Copyright (C) 2011, 2013, 2014, 2018, 2020, 2022 Stephane Carrez
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
package body Util.Dates is

   --  ------------------------------
   --  Split the date into a date record (See Ada.Calendar.Formatting.Split).
   --  ------------------------------
   procedure Split (Into       : out Date_Record;
                    Date       : in Ada.Calendar.Time;
                    Time_Zone  : in Ada.Calendar.Time_Zones.Time_Offset := 0) is
      use Ada.Calendar;
      D : Ada.Calendar.Time := Date;
   begin
      Into.Date      := Date;
      Into.Time_Zone := Time_Zone;
      Ada.Calendar.Formatting.Split (Date        => Date,
                                     Year        => Into.Year,
                                     Month       => Into.Month,
                                     Day         => Into.Month_Day,
                                     Hour        => Into.Hour,
                                     Minute      => Into.Minute,
                                     Second      => Into.Second,
                                     Sub_Second  => Into.Sub_Second,
                                     Leap_Second => Into.Leap_Second,
                                     Time_Zone   => Time_Zone);

      --  The Day_Of_Week function uses the local timezone to find the week day.
      --  The wrong day is computed if the timezone is different.  If the current
      --  date is 23:30 GMT and the current system timezone is GMT+2, then the computed
      --  day of week will be the next day due to the +2 hour offset (01:30 AM).
      --  To avoid the timezone issue, we virtually force the hour to 12:00 am.
      if Into.Hour > 12 then
         D := D - Duration ((Into.Hour - 12) * 3600);
      elsif Into.Hour < 12 then
         D := D + Duration ((12 - Into.Hour) * 3600);
      end if;
      D := D - Duration ((60 - Into.Minute) * 60);
      Into.Day := Ada.Calendar.Formatting.Day_Of_Week (D);
   end Split;

   --  ------------------------------
   --  Return the date from the date record (See Ada.Calendar.Formatting.Time_Of).
   --  ------------------------------
   function Time_Of (Date : in Date_Record) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Formatting.Time_Of (Year        => Date.Year,
                                              Month       => Date.Month,
                                              Day         => Date.Month_Day,
                                              Hour        => Date.Hour,
                                              Minute      => Date.Minute,
                                              Second      => Date.Second,
                                              Sub_Second  => Date.Sub_Second,
                                              Time_Zone   => Date.Time_Zone);
   end Time_Of;

   --  ------------------------------
   --  Returns true if the given year is a leap year.
   --  ------------------------------
   function Is_Leap_Year (Year : in Ada.Calendar.Year_Number) return Boolean is
   begin
      if Year mod 400 = 0 then
         return True;
      elsif Year mod 100 = 0 then
         return False;
      elsif Year mod 4 = 0 then
         return True;
      else
         return False;
      end if;
   end Is_Leap_Year;

   --  ------------------------------
   --  Returns true if both dates are on the same day.
   --  ------------------------------
   function Is_Same_Day (Date1, Date2 : in Ada.Calendar.Time) return Boolean is
      Split_Date1 : Date_Record;
      Split_Date2 : Date_Record;
   begin
      Split (Split_Date1, Date1);
      Split (Split_Date2, Date2);
      return Is_Same_Day (Split_Date1, Split_Date2);
   end Is_Same_Day;

   function Is_Same_Day (Date1, Date2 : in Date_Record) return Boolean is
   begin
      return Date1.Year = Date2.Year
        and then Date1.Month = Date2.Month
        and then Date1.Month_Day = Date2.Month_Day;
   end Is_Same_Day;

   --  ------------------------------
   --  Get the number of days in the given year.
   --  ------------------------------
   function Get_Day_Count (Year : in Ada.Calendar.Year_Number)
                           return Ada.Calendar.Arithmetic.Day_Count is
   begin
      if Is_Leap_Year (Year) then
         return 366;
      else
         return 365;
      end if;
   end Get_Day_Count;

   Month_Day_Count : constant array (Ada.Calendar.Month_Number)
     of Ada.Calendar.Arithmetic.Day_Count
       := (1 => 31, 2 => 28, 3 => 31, 4 => 30, 5 => 31, 6 => 30,
           7 => 31, 8 => 31, 9 => 30, 10 => 31, 11 => 30, 12 => 31);

   --  ------------------------------
   --  Get the number of days in the given month.
   --  ------------------------------
   function Get_Day_Count (Year  : in Ada.Calendar.Year_Number;
                           Month : in Ada.Calendar.Month_Number)
                           return Ada.Calendar.Arithmetic.Day_Count is
   begin
      if Month /= 2 then
         return Month_Day_Count (Month);
      elsif Is_Leap_Year (Year) then
         return 29;
      else
         return 28;
      end if;
   end Get_Day_Count;

   --  ------------------------------
   --  Get a time representing the given date at 00:00:00.
   --  ------------------------------
   function Get_Day_Start (Date : in Date_Record) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Formatting.Time_Of (Year        => Date.Year,
                                              Month       => Date.Month,
                                              Day         => Date.Month_Day,
                                              Hour        => 0,
                                              Minute      => 0,
                                              Second      => 0,
                                              Sub_Second  => 0.0,
                                              Time_Zone   => Date.Time_Zone);
   end Get_Day_Start;

   function Get_Day_Start (Date : in Ada.Calendar.Time) return Ada.Calendar.Time is
      D : Date_Record;
   begin
      Split (D, Date);
      return Get_Day_Start (D);
   end Get_Day_Start;

   --  ------------------------------
   --  Get a time representing the given date at 23:59:59.
   --  ------------------------------
   function Get_Day_End (Date : in Date_Record) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Formatting.Time_Of (Year        => Date.Year,
                                              Month       => Date.Month,
                                              Day         => Date.Month_Day,
                                              Hour        => 23,
                                              Minute      => 59,
                                              Second      => 59,
                                              Sub_Second  => 0.999,
                                              Time_Zone   => Date.Time_Zone);
   end Get_Day_End;

   function Get_Day_End (Date : in Ada.Calendar.Time) return Ada.Calendar.Time is
      D : Date_Record;
   begin
      Split (D, Date);
      return Get_Day_End (D);
   end Get_Day_End;

   --  ------------------------------
   --  Get a time representing the beginning of the week at 00:00:00.
   --  ------------------------------
   function Get_Week_Start (Date : in Date_Record) return Ada.Calendar.Time is
      use Ada.Calendar.Formatting;
      use Ada.Calendar.Arithmetic;

      T : constant Ada.Calendar.Time := Time_Of (Year        => Date.Year,
                                                 Month       => Date.Month,
                                                 Day         => Date.Month_Day,
                                                 Hour        => 0,
                                                 Minute      => 0,
                                                 Second      => 0,
                                                 Sub_Second  => 0.0,
                                                 Time_Zone   => Date.Time_Zone);
   begin
      if Date.Day = Ada.Calendar.Formatting.Monday then
         return T;
      else
         return T - Day_Count (Day_Name'Pos (Date.Day) - Day_Name'Pos (Monday));
      end if;
   end Get_Week_Start;

   function Get_Week_Start (Date : in Ada.Calendar.Time) return Ada.Calendar.Time is
      D : Date_Record;
   begin
      Split (D, Date);
      return Get_Week_Start (D);
   end Get_Week_Start;

   --  ------------------------------
   --  Get a time representing the end of the week at 23:59:99.
   --  ------------------------------
   function Get_Week_End (Date : in Date_Record) return Ada.Calendar.Time is
      use Ada.Calendar.Formatting;
      use Ada.Calendar.Arithmetic;

      T : constant Ada.Calendar.Time := Time_Of (Year        => Date.Year,
                                                 Month       => Date.Month,
                                                 Day         => Date.Month_Day,
                                                 Hour        => 23,
                                                 Minute      => 59,
                                                 Second      => 59,
                                                 Sub_Second  => 0.999,
                                                 Time_Zone   => Date.Time_Zone);
   begin
      --  End of week is 6 days + 23:59:59
      if Date.Day = Ada.Calendar.Formatting.Sunday then
         return T;
      else
         return T + Day_Count (6 - (Day_Name'Pos (Date.Day) - Day_Name'Pos (Monday)));
      end if;
   end Get_Week_End;

   function Get_Week_End (Date : in Ada.Calendar.Time) return Ada.Calendar.Time is
      D : Date_Record;
   begin
      Split (D, Date);
      return Get_Week_End (D);
   end Get_Week_End;

   --  ------------------------------
   --  Get a time representing the beginning of the month at 00:00:00.
   --  ------------------------------
   function Get_Month_Start (Date : in Date_Record) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Formatting.Time_Of (Year        => Date.Year,
                                              Month       => Date.Month,
                                              Day         => Ada.Calendar.Day_Number'First,
                                              Hour        => 0,
                                              Minute      => 0,
                                              Second      => 0,
                                              Sub_Second  => 0.0,
                                              Time_Zone   => Date.Time_Zone);
   end Get_Month_Start;

   function Get_Month_Start (Date : in Ada.Calendar.Time) return Ada.Calendar.Time is
      D : Date_Record;
   begin
      Split (D, Date);
      return Get_Month_Start (D);
   end Get_Month_Start;

   --  ------------------------------
   --  Get a time representing the end of the month at 23:59:59.
   --  ------------------------------
   function Get_Month_End (Date : in Date_Record) return Ada.Calendar.Time is
      Last_Day : constant Ada.Calendar.Day_Number
        := Ada.Calendar.Day_Number (Get_Day_Count (Date.Year, Date.Month));
   begin
      return Ada.Calendar.Formatting.Time_Of (Year        => Date.Year,
                                              Month       => Date.Month,
                                              Day         => Last_Day,
                                              Hour        => 23,
                                              Minute      => 59,
                                              Second      => 59,
                                              Sub_Second  => 0.999,
                                              Time_Zone   => Date.Time_Zone);
   end Get_Month_End;

   function Get_Month_End (Date : in Ada.Calendar.Time) return Ada.Calendar.Time is
      D : Date_Record;
   begin
      Split (D, Date);
      return Get_Month_End (D);
   end Get_Month_End;

end Util.Dates;
