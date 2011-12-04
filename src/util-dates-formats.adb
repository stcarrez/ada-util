-----------------------------------------------------------------------
--  util-dates-formats -- Date Format ala strftime
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

with GNAT.Calendar;

with Util.Strings.Transforms;
package body Util.Dates.Formats is

   use Ada.Strings.Unbounded;

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
      else
         --  If the resource bundle is empty, fallback to hard-coded English values.
         case Month is
         when 1 =>
            Append (Into, "January");

         when 2 =>
            Append (Into, "February");

         when 3 =>
            Append (Into, "March");

         when 4 =>
            Append (Into, "April");

         when 5 =>
            Append (Into, "May");

         when 6 =>
            Append (Into, "June");

         when 7 =>
            Append (Into, "July");

         when 8 =>
            Append (Into, "August");

         when 9 =>
            Append (Into, "September");

         when 10 =>
            Append (Into, "October");

         when 11 =>
            Append (Into, "November");

         when 12 =>
            Append (Into, "December");
         end case;
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
      else
         --  If the resource bundle is empty, fallback to hard-coded English values.
         case Day is
         when Monday =>
            Append (Into, "Monday");
         when Tuesday =>
            Append (Into, "Tuesday");
         when Wednesday =>
            Append (Into, "Wednesday");
         when Thursday =>
            Append (Into, "Thursday");
         when Friday =>
            Append (Into, "Friday");
         when Saturday =>
            Append (Into, "Saturday");
         when Sunday =>
            Append (Into, "Sunday");
         end case;
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
      if Length = 0 or (Padding /= ' ' and Padding /= '0') then
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
            if C = '_' or C = '-' or C = 'E' or C  = 'O' or C = '^' then
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

end Util.Dates.Formats;
