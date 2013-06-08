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
with Ada.Calendar.Arithmetic;
package body Util.Dates is

   --  ------------------------------
   --  Split the date into a date record (See Ada.Calendar.Formatting.Split).
   --  ------------------------------
   procedure Split (Into       : out Date_Record;
                    Date       : in Ada.Calendar.Time;
                    Time_Zone  : in Ada.Calendar.Time_Zones.Time_Offset := 0) is
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
      Into.Day := Ada.Calendar.Formatting.Day_Of_Week (Date);
   end Split;

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
      Day : constant Ada.Calendar.Formatting.Day_Name := Ada.Calendar.Formatting.Day_Of_Week (T);
   begin
      if Day = Ada.Calendar.Formatting.Monday then
         return T;
      else
         return T - Day_Count (Day_Name'Pos (Day) - Day_Name'Pos (Monday));
      end if;
   end Get_Week_Start;

   function Get_Week_Start (Date : in Ada.Calendar.Time) return Ada.Calendar.Time is
      D : Date_Record;
   begin
      Split (D, Date);
      return Get_Week_Start (D);
   end Get_Week_Start;

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

end Util.Dates;
