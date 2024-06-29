-----------------------------------------------------------------------
--  util-dates-iso8601 -- ISO8601 dates
--  Copyright (C) 2011, 2013, 2016, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Calendar;

--  == ISO8601 Dates ==
--  The ISO8601 defines a standard date format that is commonly used and easily parsed by programs.
--  The `Util.Dates.ISO8601` package provides an `Image` function to convert a date into that
--  target format and a `Value` function to parse such format string and return the date.
--
--      Now  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
--      S    : constant String := Util.Dates.ISO8601.Image (Now);
--      Date : Ada.Calendar.time := Util.Dates.ISO8601.Value (S);
--
--  A `Constraint_Error` exception is raised when the date string is not in the correct format.
package Util.Dates.ISO8601 is

   type Precision_Type is (YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, SUBSECOND);

   --  Parses an ISO8601 date and return it as a calendar time.
   --  Raises Constraint_Error if the date format is not recognized.
   function Value (Date : in String) return Ada.Calendar.Time;

   --  Return the ISO8601 date.
   function Image (Date : in Ada.Calendar.Time) return String;
   function Image (Date : in Date_Record) return String;
   function Image (Date      : in Ada.Calendar.Time;
                   Precision : in Precision_Type) return String;
   function Image (Date      : in Date_Record;
                   Precision : in Precision_Type) return String;

end Util.Dates.ISO8601;
