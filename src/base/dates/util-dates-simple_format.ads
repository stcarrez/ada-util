-----------------------------------------------------------------------
--  util-dates-simple_format -- Simple date format a la java SimpleDateFormat
--  Copyright (C) 2022 Stephane Carrez
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

--  Format the date passed in `Date` with a simple format pattern.
--  The pattern is composed of minimalist sequences that are replaced by
--  values, unrecognized characters are passed as is:
--  YYYY : year     MM : month     dd: day    HH: hour   mm: minute   ss: second
function Util.Dates.Simple_Format (Pattern : in String;
                                   Date    : in Ada.Calendar.Time) return String;
