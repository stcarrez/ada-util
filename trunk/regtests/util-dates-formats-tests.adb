-----------------------------------------------------------------------
--  util-dates-formats-tests - Test for date formats
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

with Ada.Calendar.Formatting;
with Util.Test_Caller;
with Util.Properties.Bundles;
package body Util.Dates.Formats.Tests is
   use Util.Tests;
   use Ada.Strings.Unbounded;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Dates.Formats.Format",
                       Test_Format'Access);
   end Add_Tests;

   procedure Test_Format (T : in out Test) is
      Bundle  : Util.Properties.Bundles.Manager;
      procedure Check (Pattern : in String;
                       Date    : in Ada.Calendar.Time;
                       Expect  : in String);

      procedure Check (Pattern : in String;
                       Date    : in Ada.Calendar.Time;
                       Expect  : in String) is
         Result : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Util.Dates.Formats.Format (Pattern => Pattern,
                                    Date    => Date,
                                    Bundle  => Bundle,
                                    Into    => Result);
         Util.Tests.Assert_Equals (T, Expect, To_String (Result),
                                   "Invalid result for: " & Pattern);
      end Check;

      T1 : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (1980, 1, 2, 10, 30, 23);
      T2 : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (2011, 11, 19, 0, 0, 0);
      T3 : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (2011, 11, 19, 23, 0, 0);
   begin
      Check ("%H", T1, "10");
      Check ("%H", T2, "00");
      Check ("%I", T3, "11");
      Check ("%k", T2, " 0");
      Check ("%k", T3, "23");
      Check ("%l", T2, " 0");
      Check ("%l", T3, "11");

      Check ("%r", T3, "11:00:00 PM");
      Check ("%r", T2, "00:00:00 AM");
      Check ("%R:%S", T3, "23:00:00");

      Check ("%y-%Y %m/%d %T", T1, "80-1980 01/02 10:30:23");
      Check ("%C %d %D", T1, "19 02 01/02/80");
      Check ("%e", T1, " 1");
      Check ("%F", T1, "1980-01-02");
      Check ("%G", T1, "1980W01");
      Check ("%g", T1, "80W01");

   end Test_Format;

end Util.Dates.Formats.Tests;
