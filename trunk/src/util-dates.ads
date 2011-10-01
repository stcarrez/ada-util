-----------------------------------------------------------------------
--  util-dates -- Date utilities
--  Copyright (C) 2011 Stephane Carrez
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
with Ada.Calendar.Time_Zones;
package Util.Dates is

   --  The Unix equivalent of 'struct tm'.
   type Date_Record is record
      Year        : Ada.Calendar.Year_Number;
      Month       : Ada.Calendar.Month_Number;
      Month_Day   : Ada.Calendar.Day_Number;
      Day         : Ada.Calendar.Formatting.Day_Name;
      Hour        : Ada.Calendar.Formatting.Hour_Number;
      Minute      : Ada.Calendar.Formatting.Minute_Number;
      Second      : Ada.Calendar.Formatting.Second_Number;
      Sub_Second  : Ada.Calendar.Formatting.Second_Duration;
      Leap_Second : Boolean;
   end record;

   --  Split the date into a date record (See Ada.Calendar.Formatting.Split).
   procedure Split (Into       : out Date_Record;
                    Date       : in Ada.Calendar.Time;
                    Time_Zone  : in Ada.Calendar.Time_Zones.Time_Offset := 0);

end Util.Dates;
