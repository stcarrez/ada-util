-----------------------------------------------------------------------
--  util-dates-simple_format -- Simple date format a la java SimpleDateFormat
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  Format the date passed in `Date` with a simple format pattern.
--  The pattern is composed of minimalist sequences that are replaced by
--  values, unrecognized characters are passed as is:
--  YYYY : year     MM : month     dd: day    HH: hour   mm: minute   ss: second
function Util.Dates.Simple_Format (Pattern : in String;
                                   Date    : in Ada.Calendar.Time) return String;
