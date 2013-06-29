-----------------------------------------------------------------------
--  util-dates -- Date utilities
--  Copyright (C) 2011, 2013 Stephane Carrez
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
package Util.Dates is

   --  The Unix equivalent of 'struct tm'.
   type Date_Record is record
      Date        : Ada.Calendar.Time;
      Year        : Ada.Calendar.Year_Number;
      Month       : Ada.Calendar.Month_Number;
      Month_Day   : Ada.Calendar.Day_Number;
      Day         : Ada.Calendar.Formatting.Day_Name;
      Hour        : Ada.Calendar.Formatting.Hour_Number;
      Minute      : Ada.Calendar.Formatting.Minute_Number;
      Second      : Ada.Calendar.Formatting.Second_Number;
      Sub_Second  : Ada.Calendar.Formatting.Second_Duration;
      Time_Zone   : Ada.Calendar.Time_Zones.Time_Offset;
      Leap_Second : Boolean;
   end record;

   --  Split the date into a date record (See Ada.Calendar.Formatting.Split).
   procedure Split (Into       : out Date_Record;
                    Date       : in Ada.Calendar.Time;
                    Time_Zone  : in Ada.Calendar.Time_Zones.Time_Offset := 0);

   --  Returns true if the given year is a leap year.
   function Is_Leap_Year (Year : in Ada.Calendar.Year_Number) return Boolean;

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
