-----------------------------------------------------------------------
--  util-dates-iso8601 -- ISO8601 dates
--  Copyright (C) 2011, 2013, 2015 Stephane Carrez
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

package body Util.Dates.ISO8601 is

   --  ------------------------------
   --  Parses an ISO8601 date and return it as a calendar time.
   --  Raises Constraint_Error if the date format is not recognized.
   --  ------------------------------
   function Value (Date : in String) return Ada.Calendar.Time is
      use Ada.Calendar;

      Result : Date_Record;
   begin
      if Date'Length < 4 then
         raise Constraint_Error with "Invalid date";
      end if;
      Result.Year := Year_Number'Value (Date (Date'First .. Date'First + 3));
      if Date'Length = 4 then
         --  ISO8601 date: YYYY
         Result.Month := 1;
         Result.Month_Day := 1;

      elsif Date'Length = 7 and Date (Date'First + 4) = '-' then
         --  ISO8601 date: YYYY-MM
         Result.Month := Month_Number'Value (Date (Date'First + 4 .. Date'Last));
         Result.Month_Day := 1;

      elsif Date'Length = 8 then
         --  ISO8601 date: YYYYMMDD
         Result.Month := Month_Number'Value (Date (Date'First + 4 .. Date'First + 5));
         Result.Month_Day := Day_Number'Value (Date (Date'First + 6 .. Date'First + 7));

      elsif Date'Length >= 9 and then Date (Date'First + 4) = '-'
        and then Date (Date'First + 7) = '-'
      then
         --  ISO8601 date: YYYY-MM-DD
         Result.Month := Month_Number'Value (Date (Date'First + 5 .. Date'First + 6));
         Result.Month_Day := Day_Number'Value (Date (Date'First + 8 .. Date'First + 9));

      else
         raise Constraint_Error with "invalid date";
      end if;
      return Ada.Calendar.Formatting.Time_Of (Year        => Result.Year,
                                              Month       => Result.Month,
                                              Day         => Result.Month_Day,
                                              Hour        => 0,
                                              Minute      => 0,
                                              Second      => 0,
                                              Sub_Second  => 0.0);
   end Value;

   --  ------------------------------
   --  Return the ISO8601 date.
   --  ------------------------------
   function Image (Date : in Ada.Calendar.Time) return String is
      D : Date_Record;
   begin
      Split (D, Date);
      return Image (D);
   end Image;

   function Image (Date : in Date_Record) return String is
      To_Char : constant array (0 .. 9) of Character := "0123456789";
      Result  : String (1 .. 10) := "0000-00-00";
   begin
      Result (1) := To_Char (Date.Year / 1000);
      Result (2) := To_Char (Date.Year / 100 mod 10);
      Result (3) := To_Char (Date.Year / 10 mod 10);
      Result (4) := To_Char (Date.Year mod 10);
      Result (6) := To_Char (Date.Month / 10);
      Result (7) := To_Char (Date.Month mod 10);
      Result (9) := To_Char (Date.Month_Day / 10);
      Result (10) := To_Char (Date.Month_Day mod 10);
      return Result;
   end Image;

end Util.Dates.ISO8601;
