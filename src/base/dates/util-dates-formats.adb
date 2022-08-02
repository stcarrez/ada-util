-----------------------------------------------------------------------
--  util-dates-formats -- Date Format ala strftime
--  Copyright (C) 2011, 2018, 2020, 2022 Stephane Carrez
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
with Ada.Containers;
with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Calendar.Formatting;

with GNAT.Calendar;

with Util.Strings.Vectors;
with Util.Strings.Transforms;
package body Util.Dates.Formats is

   use Ada.Strings.Unbounded;

   type String_Array is array (Positive range <>) of Util.Strings.Name_Access;

   --  Week day names (long and short).
   Monday_Name          : aliased constant String := "Monday";
   Monday_Short_Name    : aliased constant String := "Mon";
   Tuesday_Name         : aliased constant String := "Tuesday";
   Tuesday_Short_Name   : aliased constant String := "Tue";
   Wednesday_Name       : aliased constant String := "Wednesday";
   Wednesday_Short_Name : aliased constant String := "Wed";
   Thursday_Name        : aliased constant String := "Thursday";
   Thursday_Short_Name  : aliased constant String := "Thu";
   Friday_Name          : aliased constant String := "Friday";
   Friday_Short_Name    : aliased constant String := "Fri";
   Saturday_Name        : aliased constant String := "Saturday";
   Saturday_Short_Name  : aliased constant String := "Sat";
   Sunday_Name          : aliased constant String := "Sunday";
   Sunday_Short_Name    : aliased constant String := "Sun";

   --  Month names (long and short).
   January_Name         : aliased constant String := "January";
   January_Short_Name   : aliased constant String := "Jan";
   February_Name        : aliased constant String := "February";
   February_Short_Name  : aliased constant String := "Feb";
   March_Name           : aliased constant String := "March";
   March_Short_Name     : aliased constant String := "Mar";
   April_Name           : aliased constant String := "April";
   April_Short_Name     : aliased constant String := "Apr";
   May_Name             : aliased constant String := "May";
   May_Short_Name       : aliased constant String := "May";
   June_Name            : aliased constant String := "June";
   June_Short_Name      : aliased constant String := "Jun";
   July_Name            : aliased constant String := "July";
   July_Short_Name      : aliased constant String := "Jul";
   August_Name          : aliased constant String := "August";
   August_Short_Name    : aliased constant String := "Aug";
   September_Name       : aliased constant String := "September";
   September_Short_Name : aliased constant String := "Sep";
   October_Name         : aliased constant String := "October";
   October_Short_Name   : aliased constant String := "Oct";
   November_Name        : aliased constant String := "November";
   November_Short_Name  : aliased constant String := "Nov";
   December_Name        : aliased constant String := "December";
   December_Short_Name  : aliased constant String := "Dec";

   Day_Names          : constant String_Array (1 .. 7)
     := (Monday_Name'Access, Tuesday_Name'Access, Wednesday_Name'Access,
         Thursday_Name'Access, Friday_Name'Access, Saturday_Name'Access, Sunday_Name'Access);

   Day_Short_Names    : constant String_Array (1 .. 7)
     := (Monday_Short_Name'Access, Tuesday_Short_Name'Access, Wednesday_Short_Name'Access,
         Thursday_Short_Name'Access, Friday_Short_Name'Access,
         Saturday_Short_Name'Access, Sunday_Short_Name'Access);

   Month_Names        : constant String_Array (1 .. 12)
     := (January_Name'Access, February_Name'Access, March_Name'Access,
         April_Name'Access, May_Name'Access, June_Name'Access,
         July_Name'Access, August_Name'Access, September_Name'Access,
         October_Name'Access, November_Name'Access, December_Name'Access);

   Month_Short_Names  : constant String_Array (1 .. 12)
     := (January_Short_Name'Access, February_Short_Name'Access, March_Short_Name'Access,
         April_Short_Name'Access, May_Short_Name'Access, June_Short_Name'Access,
         July_Short_Name'Access, August_Short_Name'Access, September_Short_Name'Access,
         October_Short_Name'Access, November_Short_Name'Access, December_Short_Name'Access);

   function Get_Label (Bundle : in Util.Properties.Manager'Class;
                       Prefix : in String;
                       Index  : in Natural;
                       Short  : in Boolean) return String;

   function Get_Label (Bundle : in Util.Properties.Manager'Class;
                       Prefix : in String;
                       Index  : in Natural;
                       Short  : in Boolean) return String is
      Num  : constant String := Natural'Image (Index);
      Name : constant String := Prefix & Num (Num'First + 1 .. Num'Last);
   begin
      if Short then
         return Bundle.Get (Name & SHORT_SUFFIX, "");
      else
         return Bundle.Get (Name & LONG_SUFFIX, "");
      end if;
   end Get_Label;

   --  ------------------------------
   --  Append the localized month string in the <b>Into</b> string.
   --  The month string is found in the resource bundle under the name:
   --    util.month<month number>.short
   --    util.month<month number>.long
   --  If the month string is not found, the month is displayed as a number.
   --  ------------------------------
   procedure Append_Month (Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                           Month  : in Ada.Calendar.Month_Number;
                           Bundle : in Util.Properties.Manager'Class;
                           Short  : in Boolean := True) is
      Value : constant String := Get_Label (Bundle, MONTH_NAME_PREFIX, Natural (Month), Short);
   begin
      if Value'Length > 0 then
         Append (Into, Value);
      elsif Short then
         Append (Into, Month_Short_Names (Month).all);
      else
         --  If the resource bundle is empty, fallback to hard-coded English values.
         Append (Into, Month_Names (Month).all);
      end if;
   end Append_Month;

   --  ------------------------------
   --  Append the localized month string in the <b>Into</b> string.
   --  The month string is found in the resource bundle under the name:
   --    util.month<month number>.short
   --    util.month<month number>.long
   --  If the month string is not found, the month is displayed as a number.
   --  ------------------------------
   procedure Append_Day (Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                         Day    : in Ada.Calendar.Formatting.Day_Name;
                         Bundle : in Util.Properties.Manager'Class;
                         Short  : in Boolean := True) is
      use Ada.Calendar.Formatting;
      Value : constant String := Get_Label (Bundle, DAY_NAME_PREFIX, Day_Name'Pos (Day), Short);
   begin
      if Value'Length > 0 then
         Append (Into, Value);
      elsif Short then
         --  If the resource bundle is empty, fallback to hard-coded English values.
         Append (Into, Day_Short_Names (Day_Name'Pos (Day)).all);
      else
         Append (Into, Day_Names (Day_Name'Pos (Day)).all);
      end if;
   end Append_Day;

   --  ------------------------------
   --  Append a number with padding if necessary
   --  ------------------------------
   procedure Append_Number (Into    : in out Ada.Strings.Unbounded.Unbounded_String;
                            Value   : in Natural;
                            Padding : in Character;
                            Length  : in Natural := 2) is
      N : constant String := Natural'Image (Value);
   begin
      if Length = 0 or else (Padding /= ' ' and then Padding /= '0') then
         Append (Into, N (N'First + 1 .. N'Last));
      elsif N'Length <= Length then
         Append (Into, Padding);
         Append (Into, N (N'First + 1 .. N'Last));
      else
         Append (Into, N (N'Last - Length + 1 .. N'Last));
      end if;
   end Append_Number;

   --  ------------------------------
   --  Append the timezone offset
   --  ------------------------------
   procedure Append_Time_Offset (Into      : in out Ada.Strings.Unbounded.Unbounded_String;
                                 Offset    : in Ada.Calendar.Time_Zones.Time_Offset) is
      use type Ada.Calendar.Time_Zones.Time_Offset;
      Value : Natural;
   begin
      if Offset < 0 then
         Append (Into, '-');
         Value := Natural (-Offset);
      else
         Value := Natural (Offset);
      end if;
      Append_Number (Into, Value / 60, '0');
      Append (Into, ':');
      Append_Number (Into, Value mod 60, '0');
   end Append_Time_Offset;

   --  ------------------------------
   --  Format the date passed in <b>Date</b> using the date pattern specified in <b>Pattern</b>.
   --  For month and day of week strings, use the resource bundle passed in <b>Bundle</b>.
   --  Returns the formatted date in the stream.
   --  ------------------------------
   procedure Format (Into      : in out Ada.Strings.Unbounded.Unbounded_String;
                     Pattern   : in String;
                     Date      : in Ada.Calendar.Time;
                     Bundle    : in Util.Properties.Manager'Class) is
      TM : Date_Record;
   begin
      Split (TM, Date);
      Format (Into, Pattern, TM, Bundle);
   end Format;

   --  ------------------------------
   --  Format the date passed in <b>Date</b> using the date pattern specified in <b>Pattern</b>.
   --  For month and day of week strings, use the resource bundle passed in <b>Bundle</b>.
   --  Returns the formatted date in the stream.
   --  ------------------------------
   procedure Format (Into      : in out Ada.Strings.Unbounded.Unbounded_String;
                     Pattern   : in String;
                     Date      : in Date_Record;
                     Bundle    : in Util.Properties.Manager'Class) is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      use Util.Strings.Transforms;
      use type Ada.Calendar.Time_Zones.Time_Offset;

      Pos : Positive  := Pattern'First;
      Pad : Character := '0';
      C   : Character;
   begin
      while Pos <= Pattern'Last loop
         C := Pattern (Pos);
         if C /= '%' then
            Append (Into, C);
            Pos := Pos + 1;
         else
            Pos := Pos + 1;
            exit when Pos > Pattern'Last;
            C := Pattern (Pos);
            Pad := '0';
            if C in '_' | '-' | 'E' | 'O' | '^' then
               exit when Pos = Pattern'Last;
               if C = '-' then
                  Pad := '-';
               elsif C = '_' then
                  Pad := ' ';
               end if;
               Pos := Pos + 1;
               C := Pattern (Pos);
            end if;
            case C is
               when '%' =>
                  Append (Into, '%');

                  --  %a     The abbreviated weekday name according to the current locale.
               when 'a' =>
                  Append_Day (Into, Date.Day, Bundle, True);

                  --  %A     The full weekday name according to the current locale.
               when 'A' =>
                  Append_Day (Into, Date.Day, Bundle, False);

                  --  %b     The abbreviated month name according to the current locale.
                  --  %h     Equivalent to %b.  (SU)
               when 'b' | 'h' =>
                  Append_Month (Into, Date.Month, Bundle, True);

                  --  %B     The full month name according to the current locale.
               when 'B' =>
                  Append_Month (Into, Date.Month, Bundle, False);

                  --  %c     The preferred date and time representation for the current locale.
               when 'c' =>
                  Format (Into, Bundle.Get (DATE_TIME_LOCALE_NAME, DATE_TIME_DEFAULT_PATTERN),
                          Date, Bundle);

                  --  %C     The century number (year/100) as a 2-digit integer. (SU)
               when 'C' =>
                  Append_Number (Into, Natural (Date.Year / 100), Pad);

                  --  %d     The day of the month as a decimal number (range 01 to 31).
               when 'd' =>
                  Append_Number (Into, Natural (Date.Month_Day), Pad);

                  --  %D     Equivalent to %m/%d/%y
               when 'D' =>
                  Format (Into, "%m/%d/%y", Date, Bundle);

                  --  %e     Like %d, the day of the month as a decimal number,
                  --  but a leading zero is replaced by a space. (SU)
               when 'e' =>
                  Append_Number (Into, Natural (Date.Month), ' ');

                  --  %F     Equivalent to %Y-%m-%d (the ISO 8601 date format). (C99)
               when 'F' =>
                  Format (Into, "%Y-%m-%d", Date, Bundle);

                  --  %G     The ISO 8601 week-based year
               when 'G' =>
                  Append_Number (Into, Natural (Date.Year), Pad, 4);
                  Append (Into, 'W');
                  Append_Number (Into, Natural (GNAT.Calendar.Week_In_Year (Date.Date)), Pad);

                  --  %g     Like %G, but without century, that is,
                  --  with a 2-digit year (00-99). (TZ)
               when 'g' =>
                  Append_Number (Into, Natural (Date.Year mod 100), Pad, 2);
                  Append (Into, 'W');
                  Append_Number (Into, Natural (GNAT.Calendar.Week_In_Year (Date.Date)), Pad);

                  --  %H     The hour as a decimal number using a 24-hour clock (range 00 to 23).
               when 'H' =>
                  Append_Number (Into, Natural (Date.Hour), Pad);

                  --  %I     The hour as a decimal number using a 12-hour clock (range 01 to 12).
               when 'I' =>
                  Append_Number (Into, Natural (Date.Hour mod 12), Pad);

                  --  %j     The day of the year as a decimal number (range 001 to 366).
               when 'j' =>
                  Append_Number (Into, Natural (GNAT.Calendar.Day_In_Year (Date.Date)), Pad, 3);

                  --  %k     The hour (24-hour clock) as a decimal number (range 0 to 23);
               when 'k' =>
                  Append_Number (Into, Natural (Date.Hour), ' ');

                  --  %l     The  hour (12-hour clock) as a decimal number (range 1 to 12);
               when 'l' =>
                  Append_Number (Into, Natural (Date.Hour mod 12), ' ');

                  --  %m     The month as a decimal number (range 01 to 12).
               when 'm' =>
                  Append_Number (Into, Natural (Date.Month), Pad);

                  --  %M     The minute as a decimal number (range 00 to 59).
               when 'M' =>
                  Append_Number (Into, Natural (Date.Minute), Pad);

                  --  %n     A newline character. (SU)
               when 'n' =>
                  Append (Into, ASCII.LF);

                  --  %p     Either "AM" or "PM"
               when 'p' =>
                  if Date.Hour >= 12 then
                     Append (Into, Bundle.Get (PM_NAME, PM_DEFAULT));
                  else
                     Append (Into, Bundle.Get (AM_NAME, AM_DEFAULT));
                  end if;

                  --  %P     Like %p but in lowercase: "am" or "pm"
               when 'P' =>
                  --  SCz 2011-10-01: the To_Lower_Case will not work for UTF-8 strings.
                  if Date.Hour >= 12 then
                     Append (Into, To_Lower_Case (Bundle.Get (PM_NAME, PM_DEFAULT)));
                  else
                     Append (Into, To_Lower_Case (Bundle.Get (AM_NAME, AM_DEFAULT)));
                  end if;

                  --  %r     The time in a.m. or p.m. notation.
                  --  In the POSIX locale this is equivalent to %I:%M:%S %p.  (SU)
               when 'r' =>
                  Format (Into, "%I:%M:%S %p", Date, Bundle);

                  --  %R     The time in 24-hour notation (%H:%M).
               when 'R' =>
                  Format (Into, "%H:%M", Date, Bundle);

                  --  %s     The number of seconds since the Epoch, that is,
                  --  since 1970-01-01 00:00:00 UTC. (TZ)
               when 's' =>
                  null;

                  --  %S     The  second as a decimal number (range 00 to 60).
               when 'S' =>
                  Append_Number (Into, Natural (Date.Second), Pad);

                  --  %t     A tab character. (SU)
               when 't' =>
                  Append (Into, ASCII.HT);

                  --  %T     The time in 24-hour notation (%H:%M:%S). (SU)
               when 'T' =>
                  Format (Into, "%H:%M:%S", Date, Bundle);

                  --  %u     The day of the week as a decimal, range 1 to 7,
                  --  Monday being 1.  See also %w.  (SU)
               when 'u' =>
                  Append_Number (Into, Day_Name'Pos (Date.Day), Pad);

                  --  %U     The week number of the current year as a decimal number,
                  --  range 00 to 53
               when 'U' =>
                  Append_Number (Into, Natural (GNAT.Calendar.Week_In_Year (Date.Date)), Pad);

                  --  %V     The  ISO 8601 week number
               when 'V' =>
                  Append_Number (Into, Natural (GNAT.Calendar.Week_In_Year (Date.Date)), Pad);

                  --  %w     The day of the week as a decimal, range 0 to 6, Sunday being 0.
                  --  See also %u.
               when 'w' =>
                  if Date.Day = Sunday then
                     Append_Number (Into, 0, Pad);
                  else
                     Append_Number (Into, Day_Name'Pos (Date.Day) + 1, Pad);
                  end if;

                  --  %W     The week number of the current year as a decimal number,
                  --  range 00 to 53
               when 'W' =>
                  Append_Number (Into, Natural (GNAT.Calendar.Week_In_Year (Date.Date)), Pad);

                  --  %x     The preferred date representation for the current locale without
                  --  the time.
               when 'x' =>
                  Format (Into, Bundle.Get (DATE_LOCALE_NAME, DATE_DEFAULT_PATTERN),
                          Date, Bundle);

                  --  %X     The preferred time representation for the current locale without
                  --  the date.
               when 'X' =>
                  Format (Into, Bundle.Get (TIME_LOCALE_NAME, TIME_DEFAULT_PATTERN),
                          Date, Bundle);

                  --  %y     The year as a decimal number without a century (range 00 to 99).
               when 'y' =>
                  Append_Number (Into, Natural (Date.Year mod 100), Pad);

                  --  %Y     The year as a decimal number including the century.
               when 'Y' =>
                  Append_Number (Into, Natural (Date.Year), Pad, 4);

                  --  %z     The time-zone as hour offset from GMT.
               when 'z' =>
                  Append_Time_Offset (Into, Date.Time_Zone);

                  --  %Z     The timezone or name or abbreviation.
               when 'Z' =>
                  Append (Into, "UTC");
                  if Date.Time_Zone > 0 then
                     Append (Into, '+');
                     Append_Time_Offset (Into, Date.Time_Zone);
                  elsif Date.Time_Zone < 0 then
                     Append_Time_Offset (Into, Date.Time_Zone);
                  end if;

               when others =>
                  Append (Into, '%');
                  Append (Into, Pattern (Pos));

            end case;
            Pos := Pos + 1;
         end if;
      end loop;
   end Format;

   function Format (Pattern   : in String;
                    Date      : in Ada.Calendar.Time;
                    Bundle    : in Util.Properties.Manager'Class) return String is
      Result : Unbounded_String;
   begin
      Format (Result, Pattern, Date, Bundle);
      return To_String (Result);
   end Format;

   procedure Parse (Date      : in String;
                    Pattern   : in String;
                    Bundle    : in Util.Properties.Manager'Class;
                    Result    : in out Date_Record;
                    Last      : out Positive);

   function Parse (Date      : in String;
                   Pattern   : in String;
                   Bundle    : in Util.Properties.Manager'Class) return Date_Record is
      Last   : Positive;
      Result : Date_Record;
   begin
      Parse (Date, Pattern, Bundle, Result, Last);
      if Last <= Date'Last then
         raise Constraint_Error with "Invalid date format";
      end if;
      Result.Date := Time_Of (Result);
      return Result;
   end Parse;

   --  ------------------------------
   --  Format the date passed in <b>Date</b> using the date pattern specified in <b>Pattern</b>.
   --  For month and day of week strings, use the resource bundle passed in <b>Bundle</b>.
   --  Returns the formatted date in the stream.
   --  ------------------------------
   procedure Parse (Date      : in String;
                    Pattern   : in String;
                    Bundle    : in Util.Properties.Manager'Class;
                    Result    : in out Date_Record;
                    Last      : out Positive) is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      use Ada.Calendar.Time_Zones;
      use Util.Strings.Transforms;
      use type Ada.Containers.Count_Type;

      procedure Expect (C : in Character);
      function Expect (List : in Util.Strings.Vectors.Vector) return Natural;
      function Parse_Number (Min : in Natural;
                             Max : in Natural) return Natural;
      procedure Load (List    : in out Util.Strings.Vectors.Vector;
                      First   : in Natural;
                      Last    : in Natural;
                      Prefix  : in String;
                      Short   : in Boolean;
                      Default : in String_Array);
      function Parse_Short_Day return Formatting.Day_Name;
      function Parse_Long_Day return Formatting.Day_Name;
      function Parse_Short_Month return Month_Number;
      function Parse_Long_Month return Month_Number;
      function Check_Match (Value : in String;
                            Upper : in Boolean) return Boolean;
      procedure Parse_AM_PM (Upper : in Boolean);

      Pattern_Pos    : Positive := Pattern'First;
      Pos            : Natural := Date'First;
      C              : Character;
      Short_Months   : Util.Strings.Vectors.Vector;
      Long_Months    : Util.Strings.Vectors.Vector;
      Short_Days     : Util.Strings.Vectors.Vector;
      Long_Days      : Util.Strings.Vectors.Vector;

      procedure Expect (C : in Character) is
      begin
         if Date (Pos) /= C then
            raise Constraint_Error with "Invalid date format at" & Natural'Image (Pos);
         end if;
         Pos := Pos + 1;
      end Expect;

      function Expect (List : in Util.Strings.Vectors.Vector) return Natural is
         Index : Natural := 0;
      begin
         for S of List loop
            if Pos + S'Length - 1 <= Date'Last and then S = Date (Pos .. Pos + S'Length - 1) then
               Pos := Pos + S'Length;
               return Index;
            end if;
            Index := Index + 1;
         end loop;
         raise Constraint_Error with "Invalid date format at" & Natural'Image (Pos);
      end Expect;

      procedure Load (List    : in out Util.Strings.Vectors.Vector;
                      First   : in Natural;
                      Last    : in Natural;
                      Prefix  : in String;
                      Short   : in Boolean;
                      Default : in String_Array) is
         Offset : constant Natural := Default'First - First;
      begin
         if List.Length = 0 then
            for I in First .. Last loop
               declare
                  Name : constant String := Get_Label (Bundle, Prefix, I, Short);
               begin
                  if Name'Length = 0 then
                     List.Append (Default (I + Offset).all);
                  else
                     List.Append (Name);
                  end if;
               end;
            end loop;
         end if;
      end Load;

      function Parse_Short_Day return Formatting.Day_Name is
      begin
         Load (Short_Days, 0, 6, DAY_NAME_PREFIX, True, Day_Short_Names);
         return Formatting.Day_Name'Val (Expect (Short_Days));
      end Parse_Short_Day;

      function Parse_Long_Day return Formatting.Day_Name is
      begin
         Load (Long_Days, 0, 6, DAY_NAME_PREFIX, False, Day_Names);
         return Formatting.Day_Name'Val (Expect (Long_Days));
      end Parse_Long_Day;

      function Parse_Short_Month return Month_Number is
      begin
         Load (Short_Months, 1, 12, MONTH_NAME_PREFIX, True, Month_Short_Names);
         return Month_Number (Expect (Short_Months) + 1);
      end Parse_Short_Month;

      function Parse_Long_Month return Month_Number is
      begin
         Load (Long_Months, 1, 12, MONTH_NAME_PREFIX, False, Month_Names);
         return Month_Number (Expect (Long_Months) + 1);
      end Parse_Long_Month;

      function Parse_Number (Min : in Natural;
                             Max : in Natural) return Natural is
         Value : Natural := 0;
      begin
         if Date (Pos) < '0' or else Date (Pos) > '9' then
            raise Constraint_Error with "Invalid date format: expecting integer";
         end if;
         while (10 * Value) < Max and then Pos <= Date'Last
           and then Date (Pos) >= '0' and then Date (Pos) <= '9' loop
            Value := Value * 10;
            Value := Value + Character'Pos (Date (Pos)) - Character'Pos ('0');
            Pos := Pos + 1;
         end loop;
         if Value < Min or else Value > Max then
            raise Constraint_Error with "Invalid date format: out of range";
         end if;
         return Value;
      end Parse_Number;

      function Check_Match (Value : in String;
                            Upper : in Boolean) return Boolean is
      begin
         if Pos + Value'Length - 1 > Date'Last then
            return False;
         elsif Upper then
            return Date (Pos .. Pos + Value'Length - 1) = Value;
         else
            return Date (Pos .. Pos + Value'Length - 1) = To_Lower_Case (Value);
         end if;
      end Check_Match;

      procedure Parse_AM_PM (Upper : in Boolean) is
         AM : constant String := Bundle.Get (AM_NAME, AM_DEFAULT);
      begin
         if Check_Match (AM, Upper) then
            Pos := Pos + AM'Length;
         else
            declare
               PM : constant String := Bundle.Get (PM_NAME, PM_DEFAULT);
            begin
               if Check_Match (PM, Upper) then
                  Pos := Pos + PM'Length;
                  Result.Hour := Result.Hour + 12;
               else
                  raise Constraint_Error with "Invalid date format: expecting am or pm";
               end if;
            end;
         end if;
      end Parse_AM_PM;

      Century     : Integer := -1;
      Value       : Integer;
      Week_Number : Integer := -1;
      pragma Unreferenced (Week_Number, Century);
   begin
      while Pattern_Pos <= Pattern'Last and then Pos <= Date'Last loop
         C := Pattern (Pattern_Pos);
         if C = ' ' then
            Pattern_Pos := Pattern_Pos + 1;
            while Pos <= Date'Last and then Ada.Characters.Handling.Is_Space (Date (Pos)) loop
               Pos := Pos + 1;
            end loop;
         elsif C /= '%' then
            Expect (C);
            Pattern_Pos := Pattern_Pos + 1;
         else
            Pattern_Pos := Pattern_Pos + 1;
            exit when Pattern_Pos > Pattern'Last;
            C := Pattern (Pattern_Pos);
            if C in '_' | '-' | 'E' | 'O' | '^' then
               exit when Pattern_Pos = Pattern'Last;
               Pattern_Pos := Pattern_Pos + 1;
               C := Pattern (Pattern_Pos);
            end if;
            case C is
               when '%' =>
                  Expect ('%');

                  --  %a     The abbreviated weekday name according to the current locale.
               when 'a' =>
                  Result.Day := Parse_Short_Day;

                  --  %A     The full weekday name according to the current locale.
               when 'A' =>
                  Result.Day := Parse_Long_Day;

                  --  %b     The abbreviated month name according to the current locale.
                  --  %h     Equivalent to %b.  (SU)
               when 'b' | 'h' =>
                  Result.Month := Parse_Short_Month;

                  --  %B     The full month name according to the current locale.
               when 'B' =>
                  Result.Month := Parse_Long_Month;

                  --  %c     The preferred date and time representation for the current locale.
               when 'c' =>
                  Parse (Date (Pos .. Date'Last),
                         Bundle.Get (DATE_TIME_LOCALE_NAME, DATE_TIME_DEFAULT_PATTERN),
                         Bundle, Result, Pos);

                  --  %C     The century number (year/100) as a 2-digit integer. (SU)
               when 'C' =>
                  Century := 100 * Parse_Number (Min => 1, Max => 99);

                  --  %d     The day of the month as a decimal number (range 01 to 31).
               when 'd' =>
                  Result.Month_Day := Day_Number (Parse_Number (Min => 1, Max => 31));

                  --  %D     Equivalent to %m/%d/%y
               when 'D' =>
                  Parse (Date (Pos .. Date'Last), "%m/%d/%y", Bundle, Result, Pos);

                  --  %e     Like %d, the day of the month as a decimal number,
                  --  but a leading zero is replaced by a space. (SU)
               when 'e' =>
                  Result.Month := Month_Number (Parse_Number (Min => 1, Max => 31));

                  --  %F     Equivalent to %Y-%m-%d (the ISO 8601 date format). (C99)
               when 'F' =>
                  Parse (Date (Pos .. Date'Last), "%Y-%m-%d", Bundle, Result, Pos);

                  --  %G     The ISO 8601 week-based year
               when 'G' =>
                  Parse (Date (Pos .. Date'Last), "%YW%W", Bundle, Result, Pos);

                  --  %g     Like %G, but without century, that is,
                  --  with a 2-digit year (00-99). (TZ)
               when 'g' =>
                  Parse (Date (Pos .. Date'Last), "%yW%W", Bundle, Result, Pos);

                  --  %H     The hour as a decimal number using a 24-hour clock (range 00 to 23).
               when 'H' =>
                  Result.Hour := Formatting.Hour_Number (Parse_Number (Min => 1, Max => 23));

                  --  %I     The hour as a decimal number using a 12-hour clock (range 01 to 12).
               when 'I' =>
                  Result.Hour := Formatting.Hour_Number (Parse_Number (Min => 1, Max => 12));

                  --  %j     The day of the year as a decimal number (range 001 to 366).
               when 'j' =>
                  Value := Parse_Number (Min => 1, Max => 366);

                  --  %k     The hour (24-hour clock) as a decimal number (range 0 to 23);
               when 'k' =>
                  Result.Hour := Hour_Number (Parse_Number (Min => 0, Max => 23));

                  --  %l     The  hour (12-hour clock) as a decimal number (range 1 to 12);
               when 'l' =>
                  Result.Hour := Parse_Number (Min => 1, Max => 12);

                  --  %m     The month as a decimal number (range 01 to 12).
               when 'm' =>
                  Result.Month := Month_Number (Parse_Number (Min => 1, Max => 12));

                  --  %M     The minute as a decimal number (range 00 to 59).
               when 'M' =>
                  Result.Minute := Minute_Number (Parse_Number (Min => 0, Max => 59));

                  --  %n     A newline character. (SU)
               when 'n' =>
                  Expect (ASCII.LF);

                  --  %p     Either "AM" or "PM"
               when 'p' =>
                  Parse_AM_PM (Upper => True);

                  --  %P     Like %p but in lowercase: "am" or "pm"
               when 'P' =>
                  Parse_AM_PM (Upper => False);

                  --  %r     The time in a.m. or p.m. notation.
                  --  In the POSIX locale this is equivalent to %I:%M:%S %p.  (SU)
               when 'r' =>
                  Parse (Date (Pos .. Date'Last), "%I:%M:%S %p", Bundle, Result, Pos);

                  --  %R     The time in 24-hour notation (%H:%M).
               when 'R' =>
                  Parse (Date (Pos .. Date'Last), "%H:%M", Bundle, Result, Pos);

                  --  %s     The number of seconds since the Epoch, that is,
                  --  since 1970-01-01 00:00:00 UTC. (TZ)
               when 's' =>
                  null;

                  --  %S     The  second as a decimal number (range 00 to 60).
               when 'S' =>
                  Result.Second := Formatting.Second_Number (Parse_Number (Min => 0, Max => 59));

                  --  %t     A tab character. (SU)
               when 't' =>
                  Expect (ASCII.HT);

                  --  %T     The time in 24-hour notation (%H:%M:%S). (SU)
               when 'T' =>
                  Parse (Date (Pos .. Date'Last), "%H:%M:%S", Bundle, Result, Pos);

                  --  %u     The day of the week as a decimal, range 1 to 7,
                  --  Monday being 1.  See also %w.  (SU)
               when 'u' =>
                  Result.Day := Day_Name'Val (Parse_Number (Min => 1, Max => 7));

                  --  %U     The week number of the current year as a decimal number,
                  --  range 00 to 53
               when 'U' =>
                  Week_Number := Parse_Number (Min => 0, Max => 53);

                  --  %V     The  ISO 8601 week number
               when 'V' =>
                  Week_Number := Parse_Number (Min => 0, Max => 53);

                  --  %w     The day of the week as a decimal, range 0 to 6, Sunday being 0.
                  --  See also %u.
               when 'w' =>
                  Value := Parse_Number (Min => 0, Max => 6);
                  if Value = 0 then
                     Result.Day := Sunday;
                  else
                     Result.Day := Day_Name'Val (Value - 1);
                  end if;

                  --  %W     The week number of the current year as a decimal number,
                  --  range 00 to 53
               when 'W' =>
                  Week_Number := Parse_Number (Min => 0, Max => 53);

                  --  %x     The preferred date representation for the current locale without
                  --  the time.
               when 'x' =>
                  Parse (Date (Pos .. Date'Last),
                         Bundle.Get (DATE_LOCALE_NAME, DATE_DEFAULT_PATTERN),
                         Bundle, Result, Pos);

                  --  %X     The preferred time representation for the current locale without
                  --  the date.
               when 'X' =>
                  Parse (Date (Pos .. Date'Last),
                         Bundle.Get (TIME_LOCALE_NAME, TIME_DEFAULT_PATTERN),
                         Bundle, Result, Pos);

                  --  %y     The year as a decimal number without a century (range 00 to 99).
               when 'y' =>
                  Value := Parse_Number (Min => 0, Max => 99);
                  Value := Value + (Natural (Result.Year) / 100) * 100;
                  Result.Year := Year_Number (Value);

                  --  %Y     The year as a decimal number including the century.
               when 'Y' =>
                  Result.Year := Parse_Number (Min => 1900, Max => 9999);

                  --  %z     The time-zone as hour offset from GMT.
               when 'z' =>
                  if Date (Pos) = '+' then
                     Pos := Pos + 1;
                     Value := Parse_Number (Min => 0, Max => 12);
                     Result.Time_Zone := Time_Offset (Value * 60);
                     Expect (':');
                     Value := Parse_Number (Min => 0, Max => 59);
                     Result.Time_Zone := Result.Time_Zone + Time_Offset (Value);
                  elsif Date (Pos) = '-' then
                     Pos := Pos + 1;
                     Value := Parse_Number (Min => 0, Max => 12);
                     Result.Time_Zone := -Time_Offset (Value * 60);
                     Expect (':');
                     Value := Parse_Number (Min => 0, Max => 59);
                     Result.Time_Zone := Result.Time_Zone - Time_Offset (Value);
                  else
                     raise Constraint_Error with "Invalid date format";
                  end if;

                  --  %Z     The timezone or name or abbreviation.
               when 'Z' =>
                  Expect ('U');
                  Expect ('T');
                  Expect ('C');
                  if Date (Pos) = '+' then
                     Pos := Pos + 1;
                     Value := Parse_Number (Min => 0, Max => 12);
                     Result.Time_Zone := Time_Offset (Value * 60);
                     Expect (':');
                     Value := Parse_Number (Min => 0, Max => 59);
                     Result.Time_Zone := Result.Time_Zone + Time_Offset (Value);
                  elsif Date (Pos) = '-' then
                     Pos := Pos + 1;
                     Value := Parse_Number (Min => 0, Max => 12);
                     Result.Time_Zone := -Time_Offset (Value * 60);
                     Expect (':');
                     Value := Parse_Number (Min => 0, Max => 59);
                     Result.Time_Zone := Result.Time_Zone - Time_Offset (Value);
                  else
                     Result.Time_Zone := 0;
                  end if;

               when others =>
                  Expect ('%');
                  Expect (Pattern (Pattern_Pos));

            end case;
            Pattern_Pos := Pattern_Pos + 1;
         end if;
      end loop;
      Last := Pos;
      if Pattern_Pos <= Pattern'Last then
         raise Constraint_Error with "Invalid date format: incomplete date";
      end if;
   end Parse;

end Util.Dates.Formats;
