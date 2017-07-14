-----------------------------------------------------------------------
--  util-dates-rfc7231-- RFC7231 date format utilities
--  Copyright (C) 2015, 2017 Stephane Carrez
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

package body Util.Dates.RFC7231 is

   use Ada.Calendar;
   use Ada.Calendar.Formatting;

   Day_Names : constant array (Day_Name) of String (1 .. 3)
     := (Sunday => "Sun", Monday => "Mon", Tuesday => "Tue",
         Wednesday => "Wed", Thursday => "Thu", Friday => "Fri", Saturday => "Sat");

   Month_Names : constant array (Month_Number) of String (1 .. 3)
     := ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
         "Sep", "Oct", "Nov", "Dec");

   procedure Append_Digits (Into  : in out Util.Strings.Builders.Builder;
                            Value : in Natural);

   function Get_Day (Day : in String) return Day_Name;
   function Get_Month (Month : in String) return Month_Number;

   procedure Append_Digits (Into  : in out Util.Strings.Builders.Builder;
                            Value : in Natural) is
      use Util.Strings.Builders;
   begin
      Append (Into, Character'Val (Character'Pos ('0') + Value / 10));
      Append (Into, Character'Val (Character'Pos ('0') + Value mod 10));
   end Append_Digits;

   --  ------------------------------
   --  Append the date in RFC7231/RFC2616 format in the string builder.
   --  The date separator can be changed to '-' to generate a cookie expires date.
   --  ------------------------------
   procedure Append_Date (Into           : in out Util.Strings.Builders.Builder;
                          Date           : in Ada.Calendar.Time;
                          Date_Separator : in Character := ' ') is
      use Util.Strings.Builders;

      Wday  : constant Day_Name := Day_Of_Week (Date);
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Hour  : Hour_Number;
      Min   : Minute_Number;
      Sec   : Second_Number;
      Ssec  : Second_Duration;
   begin
      Split (Date => Date, Year => Year, Month => Month, Day => Day,
             Hour => Hour, Minute => Min, Second => Sec, Sub_Second => Ssec);
      Append (Into, Day_Names (Wday));
      Append (Into, ", ");
      Append_Digits (Into, Integer (Day));
      Append (Into, Date_Separator);
      Append (Into, Month_Names (Month));
      Append (Into, Date_Separator);
      Append_Digits (Into, Integer (Year / 100));
      Append_Digits (Into, Integer (Year mod 100));
      Append (Into, ' ');
      Append_Digits (Into, Integer (Hour));
      Append (Into, ':');
      Append_Digits (Into, Integer (Min));
      Append (Into, ':');
      Append_Digits (Into, Integer (Sec));
      Append (Into, " GMT");
   end Append_Date;

   function Get_Month (Month : in String) return Month_Number is
   begin
      for I in Month_Names'Range loop
         if Month_Names (I) = Month then
            return I;
         end if;
      end loop;
      raise Constraint_Error with "Invalid month";
   end Get_Month;

   function Get_Day (Day : in String) return Day_Name is
   begin
      for I in Day_Names'Range loop
         if Day_Names (I) = Day then
            return I;
         end if;
      end loop;
      raise Constraint_Error with "Invalid day name";
   end Get_Day;

   --  ------------------------------
   --  Parses a HTTP date that follows the RFC7231 or RFC2616 format.
   --  Raises Constraint_Error if the date format is not recognized.
   --  ------------------------------
   function Value (Date : in String) return Ada.Calendar.Time is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Hour  : Hour_Number;
      Min   : Minute_Number;
      Sec   : Second_Number;
      Wday  : Day_Name;
      Leap_Second : Boolean := False;

      pragma Unreferenced (Wday);
   begin
      if Date'Length < 10 then
         raise Constraint_Error with "Invalid date format";
      end if;
      if Date (Date'First + 3) = ',' then
         --  This is a fixed format, check for date separators.
         if Date (Date'First + 7) /= ' '
           or else Date (Date'First + 11) /= ' '
           or else Date (Date'First + 16) /= ' '
           or else Date (Date'First + 19) /= ':'
           or else Date (Date'First + 22) /= ':'
           or else Date (Date'First + 25 .. Date'Last) /= " GMT"
         then
            raise Constraint_Error with "Invalid date format";
         end if;
         Wday := Get_Day (Date (Date'First .. Date'First + 2));
         Day := Day_Number'Value (Date (Date'First + 5 .. Date'First + 6));
         Month := Get_Month (Date (Date'First + 8 .. Date'First + 10));
         Year := Year_Number'Value (Date (Date'First + 12 .. Date'First + 15));
         Hour := Hour_Number'Value (Date (Date'First + 17 .. Date'First + 18));
         Min := Minute_Number'Value (Date (Date'First + 20 .. Date'First + 21));
         if Date (Date'First + 23 .. Date'First + 24) = "60" then
            Leap_Second := True;
            Sec := 59;
         else
            Sec := Second_Number'Value (Date (Date'First + 23 .. Date'First + 24));
         end if;
      else
         raise Constraint_Error with "Invalid date format (deprecated formats not supported)";
      end if;
      return Ada.Calendar.Formatting.Time_Of (Year        => Year,
                                              Month       => Month,
                                              Day         => Day,
                                              Hour        => Hour,
                                              Minute      => Min,
                                              Second      => Sec,
                                              Sub_Second  => 0.0,
                                              Leap_Second => Leap_Second);
   end Value;

   --  ------------------------------
   --  Return the RFC7231/RFC2616 date format.
   --  ------------------------------
   function Image (Date : in Ada.Calendar.Time) return String is
      Result : Util.Strings.Builders.Builder (Len => 40);
   begin
      Append_Date (Into => Result,
                   Date => Date);
      return Util.Strings.Builders.To_Array (Result);
   end Image;

end Util.Dates.RFC7231;
