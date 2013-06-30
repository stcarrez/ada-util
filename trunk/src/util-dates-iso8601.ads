-----------------------------------------------------------------------
--  util-dates-iso8601 -- ISO8601 dates
--  Copyright (C) 2011, 2013 Stephane Carrez
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
package Util.Dates.ISO8601 is

   --  Parses an ISO8601 date and return it as a calendar time.
   --  Raises Constraint_Error if the date format is not recognized.
   function Value (Date : in String) return Ada.Calendar.Time;

   --  Return the ISO8601 date.
   function Image (Date : in Ada.Calendar.Time) return String;
   function Image (Date : in Date_Record) return String;

end Util.Dates.ISO8601;
