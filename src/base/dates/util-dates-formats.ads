-----------------------------------------------------------------------
--  util-dates-formats -- Date Format ala strftime
--  Copyright (C) 2011, 2018, 2022 Stephane Carrez
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

with Ada.Strings.Unbounded;
with Util.Properties;

--  == Localized date formatting ==
--  The `Util.Dates.Formats` provides a date formatting and parsing operation similar to the
--  Unix `strftime`, `strptime` or the `GNAT.Calendar.Time_IO`.  The localization of month
--  and day labels is however handled through `Util.Properties.Bundle` (similar to
--  the Java world).  Unlike `strftime` and `strptime`, this allows to have a multi-threaded
--  application that reports dates in several languages.  The `GNAT.Calendar.Time_IO` only
--  supports English and this is the reason why it is not used here.
--
--  The date pattern recognizes the following formats:
--
--  | Format | Description |
--  | --- | ---------- |
--  | %a  | The abbreviated weekday name according to the current locale.
--  | %A  | The full weekday name according to the current locale.
--  | %b  | The abbreviated month name according to the current locale.
--  | %h  | Equivalent to %b. (SU)
--  | %B  | The full month name according to the current locale.
--  | %c  | The preferred date and time representation for the current locale.
--  | %C  | The century number (year/100) as a 2-digit integer. (SU)
--  | %d  | The day of the month as a decimal number (range 01 to 31).
--  | %D  | Equivalent to %m/%d/%y
--  | %e  | Like %d, the day of the month as a decimal number,
--  |     | but a leading zero is replaced by a space. (SU)
--  | %F  | Equivalent to %Y\-%m\-%d (the ISO 8601 date format). (C99)
--  | %G  | The ISO 8601 week-based year
--  | %H  | The hour as a decimal number using a 24-hour clock (range 00 to 23).
--  | %I  | The hour as a decimal number using a 12-hour clock (range 01 to 12).
--  | %j  | The day of the year as a decimal number (range 001 to 366).
--  | %k  | The hour (24 hour clock) as a decimal number (range 0 to 23);
--  | %l  | The hour (12 hour clock) as a decimal number (range 1 to 12);
--  | %m  | The month as a decimal number (range 01 to 12).
--  | %M  | The minute as a decimal number (range 00 to 59).
--  | %n  | A newline character. (SU)
--  | %p  | Either "AM" or "PM"
--  | %P  | Like %p but in lowercase: "am" or "pm"
--  | %r  | The time in a.m. or p.m. notation.
--  |     | In the POSIX locale this is equivalent to %I:%M:%S %p. (SU)
--  | %R  | The time in 24 hour notation (%H:%M).
--  | %s  | The number of seconds since the Epoch, that is,
--  |     | since 1970\-01\-01 00:00:00 UTC. (TZ)
--  | %S  | The second as a decimal number (range 00 to 60).
--  | %t  | A tab character. (SU)
--  | %T  | The time in 24 hour notation (%H:%M:%S). (SU)
--  | %u  | The day of the week as a decimal, range 1 to 7,
--  |     | Monday being 1. See also %w. (SU)
--  | %U  | The week number of the current year as a decimal
--  |     | number, range 00 to 53
--  | %V  | The ISO 8601 week number
--  | %w  | The day of the week as a decimal, range 0 to 6,
--  |     | Sunday being 0. See also %u.
--  | %W  | The week number of the current year as a decimal number,
--  |     | range 00 to 53
--  | %x  | The preferred date representation for the current locale
--  |     | without the time.
--  | %X  | The preferred time representation for the current locale
--  |     | without the date.
--  | %y  | The year as a decimal number without a century (range 00 to 99).
--  | %Y  | The year as a decimal number including the century.
--  | %z  | The timezone as hour offset from GMT.
--  | %Z  | The timezone or name or abbreviation.
--
--  The following strftime flags are ignored:
--
--  | Format | Description |
--  | --- | ---------- |
--  | %E  |  Modifier: use alternative format, see below. (SU)
--  | %O  |  Modifier: use alternative format, see below. (SU)
--
--  SU:  Single Unix Specification
--  C99: C99 standard, POSIX.1-2001
--
--  See strftime (3) and strptime (3) manual page
--
--  To format and use the localize date, it is first necessary to get a bundle
--  for the `dates` so that date elements are translated into the given locale.
--
--     Factory     : Util.Properties.Bundles.Loader;
--     Bundle      : Util.Properties.Bundles.Manager;
--     ...
--        Load_Bundle (Factory, "dates", "fr", Bundle);
--
--  The date is formatted according to the pattern string described above.
--  The bundle is used by the formatter to use the day and month names in the
--  expected locale.
--
--     Date : String := Util.Dates.Formats.Format (Pattern => Pattern,
--                                                 Date    => Ada.Calendar.Clock,
--                                                 Bundle  => Bundle);
--
--  To parse a date according to a pattern and a localization, the same pattern string
--  and bundle can be used and the `Parse` function will return the date in split format.
--
--     Result : Date_Record := Util.Dates.Formats.Parse (Date    => Date,
--                                                       Pattern => Pattern,
--                                                       Bundle  => Bundle);
--
package Util.Dates.Formats is

   --  Month labels.
   MONTH_NAME_PREFIX         : constant String := "util.month";

   --  Day labels.
   DAY_NAME_PREFIX           : constant String := "util.day";

   --  Short month/day suffix.
   SHORT_SUFFIX              : constant String := ".short";

   --  Long month/day suffix.
   LONG_SUFFIX               : constant String := ".long";

   --  The date time pattern name to be used for the %x representation.
   --  This property name is searched in the bundle to find the localized date time pattern.
   DATE_TIME_LOCALE_NAME     : constant String := "util.datetime.pattern";

   --  The default date pattern for %c (English).
   DATE_TIME_DEFAULT_PATTERN : constant String := "%a %b %_d %T %Y";

   --  The date pattern to be used for the %x representation.
   --  This property name is searched in the bundle to find the localized date pattern.
   DATE_LOCALE_NAME          : constant String := "util.date.pattern";

   --  The default date pattern for %x (English).
   DATE_DEFAULT_PATTERN      : constant String := "%m/%d/%y";

   --  The time pattern to be used for the %X representation.
   --  This property name is searched in the bundle to find the localized time pattern.
   TIME_LOCALE_NAME          : constant String := "util.time.pattern";

   --  The default time pattern for %X (English).
   TIME_DEFAULT_PATTERN      : constant String := "%T %Y";

   AM_NAME                   : constant String := "util.date.am";
   PM_NAME                   : constant String := "util.date.pm";

   AM_DEFAULT                : constant String := "AM";
   PM_DEFAULT                : constant String := "PM";

   --  Format the date passed in <b>Date</b> using the date pattern specified in <b>Pattern</b>.
   --  The date pattern is similar to the Unix <b>strftime</b> operation.
   --
   --  For month and day of week strings, use the resource bundle passed in <b>Bundle</b>.
   --  Append the formatted date in the <b>Into</b> string.
   procedure Format (Into      : in out Ada.Strings.Unbounded.Unbounded_String;
                     Pattern   : in String;
                     Date      : in Date_Record;
                     Bundle    : in Util.Properties.Manager'Class);

   --  Format the date passed in <b>Date</b> using the date pattern specified in <b>Pattern</b>.
   --  For month and day of week strings, use the resource bundle passed in <b>Bundle</b>.
   --  Append the formatted date in the <b>Into</b> string.
   procedure Format (Into      : in out Ada.Strings.Unbounded.Unbounded_String;
                     Pattern   : in String;
                     Date      : in Ada.Calendar.Time;
                     Bundle    : in Util.Properties.Manager'Class);

   function Format (Pattern   : in String;
                    Date      : in Ada.Calendar.Time;
                    Bundle    : in Util.Properties.Manager'Class) return String;

   --  Append the localized month string in the <b>Into</b> string.
   --  The month string is found in the resource bundle under the name:
   --    util.month<month number>.short
   --    util.month<month number>.long
   --  If the month string is not found, the month is displayed as a number.
   procedure Append_Month (Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                           Month  : in Ada.Calendar.Month_Number;
                           Bundle : in Util.Properties.Manager'Class;
                           Short  : in Boolean := True);

   --  Append the localized month string in the <b>Into</b> string.
   --  The month string is found in the resource bundle under the name:
   --    util.month<month number>.short
   --    util.month<month number>.long
   --  If the month string is not found, the month is displayed as a number.
   procedure Append_Day (Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                         Day    : in Ada.Calendar.Formatting.Day_Name;
                         Bundle : in Util.Properties.Manager'Class;
                         Short  : in Boolean := True);

   --  Append a number with padding if necessary
   procedure Append_Number (Into    : in out Ada.Strings.Unbounded.Unbounded_String;
                            Value   : in Natural;
                            Padding : in Character;
                            Length  : in Natural := 2);

   --  Append the timezone offset
   procedure Append_Time_Offset (Into      : in out Ada.Strings.Unbounded.Unbounded_String;
                                 Offset    : in Ada.Calendar.Time_Zones.Time_Offset);

   --  Parse the date according to the pattern and the given locale bundle and
   --  return the data split record.
   --  A `Constraint_Error` exception is raised if the date string is not in the correct format.
   function Parse (Date      : in String;
                   Pattern   : in String;
                   Bundle    : in Util.Properties.Manager'Class) return Date_Record;

end Util.Dates.Formats;
