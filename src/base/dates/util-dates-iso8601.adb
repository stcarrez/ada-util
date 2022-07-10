-----------------------------------------------------------------------
--  util-dates-iso8601 -- ISO8601 dates
--  Copyright (C) 2011, 2013, 2015, 2016, 2017, 2018, 2020, 2022 Stephane Carrez
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
      use Ada.Calendar.Formatting;
      use Ada.Calendar.Time_Zones;

      Result : Date_Record;
      Pos    : Natural;
   begin
      if Date'Length < 4 then
         raise Constraint_Error with "Invalid date";
      end if;
      Result.Hour       := 0;
      Result.Minute     := 0;
      Result.Second     := 0;
      Result.Sub_Second := 0.0;
      Result.Time_Zone  := 0;
      Result.Year := Year_Number'Value (Date (Date'First .. Date'First + 3));
      if Date'Length = 4 then
         --  ISO8601 date: YYYY
         Result.Month := 1;
         Result.Month_Day := 1;

      elsif Date'Length = 7 and then Date (Date'First + 4) = '-' then
         --  ISO8601 date: YYYY-MM
         Result.Month := Month_Number'Value (Date (Date'First + 5 .. Date'Last));
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

         --  ISO8601 date: YYYY-MM-DDTHH
         if Date'Length > 12 then
            if Date (Date'First + 10) /= 'T' then
               raise Constraint_Error with "invalid date";
            end if;
            Result.Hour := Hour_Number'Value (Date (Date'First + 11 .. Date'First + 12));
            Pos := Date'First + 13;
         end if;
         if Date'Length > 15 then
            if Date (Date'First + 13) /= ':' then
               raise Constraint_Error with "invalid date";
            end if;
            Result.Minute := Minute_Number'Value (Date (Date'First + 14 .. Date'First + 15));
            Pos := Date'First + 16;
         end if;
         if Date'Length >= 17 then
            if Date (Date'First + 16) /= ':' or else Date'Length <= 18 then
               raise Constraint_Error with "invalid date";
            end if;
            Result.Second := Second_Number'Value (Date (Date'First + 17 .. Date'First + 18));
            Pos := Date'First + 19;
            if Pos <= Date'Last then
               if Date (Pos) = '.' or else Date (Pos) = ',' then
                  if Date'Length < 22 then
                     raise Constraint_Error with "invalid date";
                  end if;
                  declare
                     Value : constant Natural := Natural'Value (Date (Pos + 1 .. Pos + 3));
                  begin
                     Result.Sub_Second := Second_Duration (Duration (Value) / 1000.0);
                  end;
                  Pos := Pos + 4;
               end if;
               if Pos <= Date'Last then
                  --  ISO8601 timezone: Z
                  --  ISO8601 timezone: +hh or -hh
                  --  ISO8601 timezone: +hhmm or -hhmm
                  --  ISO8601 timezone: +hh:mm or -hh:mm
                  if Date (Pos) = 'Z' then
                     if Pos /= Date'Last then
                        raise Constraint_Error with "invalid date";
                     end if;
                  elsif Date (Pos) /= '-' and then Date (Pos) /= '+' then
                     raise Constraint_Error with "invalid date";

                  elsif Pos + 2 = Date'Last then
                     Result.Time_Zone := 60 * Time_Offset'Value (Date (Pos + 1 .. Date'Last));

                  elsif Pos + 4 = Date'Last then
                     Result.Time_Zone := 60 * Time_Offset'Value (Date (Pos + 1 .. Pos + 2))
                       + Time_Offset'Value (Date (Pos + 3 .. Date'Last));

                  elsif Pos + 5 = Date'Last and then Date (Pos + 3) = ':' then
                     Result.Time_Zone := 60 * Time_Offset'Value (Date (Pos + 1 .. Pos + 2))
                       + Time_Offset'Value (Date (Pos + 4 .. Date'Last));

                  else
                     raise Constraint_Error with "invalid date";
                  end if;
               end if;
            end if;
         end if;
      else
         raise Constraint_Error with "invalid date";
      end if;
      return Time_Of (Result);
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

   function Image (Date      : in Ada.Calendar.Time;
                   Precision : in Precision_Type) return String is
      D : Date_Record;
   begin
      Split (D, Date);
      return Image (D, Precision);
   end Image;

   function Image (Date      : in Date_Record;
                   Precision : in Precision_Type) return String is
      use type Ada.Calendar.Time_Zones.Time_Offset;

      To_Char : constant array (0 .. 9) of Character := "0123456789";
      Result  : String (1 .. 29) := "0000-00-00T00:00:00.000-00:00";
      N,  Tz  : Natural;
   begin
      Result (1) := To_Char (Date.Year / 1000);
      Result (2) := To_Char (Date.Year / 100 mod 10);
      Result (3) := To_Char (Date.Year / 10 mod 10);
      Result (4) := To_Char (Date.Year mod 10);
      if Precision = YEAR then
         return Result (1 .. 4);
      end if;
      Result (6) := To_Char (Date.Month / 10);
      Result (7) := To_Char (Date.Month mod 10);
      if Precision = MONTH then
         return Result (1 .. 7);
      end if;
      Result (9) := To_Char (Date.Month_Day / 10);
      Result (10) := To_Char (Date.Month_Day mod 10);
      if Precision = DAY then
         return Result (1 .. 10);
      end if;
      Result (12) := To_Char (Date.Hour / 10);
      Result (13) := To_Char (Date.Hour mod 10);
      if Precision = HOUR then
         return Result (1 .. 13);
      end if;
      Result (15) := To_Char (Date.Minute / 10);
      Result (16) := To_Char (Date.Minute mod 10);
      if Precision = MINUTE then
         return Result (1 .. 16);
      end if;
      Result (18) := To_Char (Date.Second / 10);
      Result (19) := To_Char (Date.Second mod 10);
      if Precision = SECOND then
         return Result (1 .. 19);
      end if;
      N := Natural (Date.Sub_Second * 1000.0);
      Result (21) := To_Char (N / 100);
      Result (22) := To_Char ((N mod 100) / 10);
      Result (23) := To_Char (N mod 10);
      if Date.Time_Zone < 0 then
         Tz := Natural (-Date.Time_Zone);
      else
         Result (24) := '+';
         Tz := Natural (Date.Time_Zone);
      end if;
      Result (25) := To_Char (Tz / 600);
      Result (26) := To_Char ((Tz / 60) mod 10);
      Tz := Tz mod 60;
      Result (28) := To_Char (Tz / 10);
      Result (29) := To_Char (Tz mod 10);
      return Result;
   end Image;

end Util.Dates.ISO8601;
