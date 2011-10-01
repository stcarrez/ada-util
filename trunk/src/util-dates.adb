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

package body Util.Dates is

   --  ------------------------------
   --  Split the date into a date record (See Ada.Calendar.Formatting.Split).
   --  ------------------------------
   procedure Split (Into       : out Date_Record;
                    Date       : in Ada.Calendar.Time;
                    Time_Zone  : in Ada.Calendar.Time_Zones.Time_Offset := 0) is
   begin
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

end Util.Dates;
