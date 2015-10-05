-----------------------------------------------------------------------
--  util-dates-rfc7231-- RFC7231 date format utilities
--  Copyright (C) 2015 Stephane Carrez
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
with Util.Strings.Builders;
package Util.Dates.RFC7231 is

   --  Parses a HTTP date that follows the RFC7231 or RFC2616 format.
   --  Raises Constraint_Error if the date format is not recognized.
   function Value (Date : in String) return Ada.Calendar.Time;

   --  Return the RFC7231/RFC2616 date format.
   function Image (Date : in Ada.Calendar.Time) return String;

   --  Append the date in RFC7231/RFC2616 format in the string builder.
   --  The date separator can be changed to '-' to generate a cookie expires date.
   procedure Append_Date (Into           : in out Util.Strings.Builders.Builder;
                          Date           : in Ada.Calendar.Time;
                          Date_Separator : in Character := ' ');

end Util.Dates.RFC7231;
