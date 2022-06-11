-----------------------------------------------------------------------
--  Util.Beans.Objects.Discretes -- Unit tests for concurrency package
--  Copyright (C) 2009, 2010, 2020, 2022 Stephane Carrez
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

with Util.Beans.Objects.Enums;
with Util.Beans.Objects.Time;
with Util.Beans.Objects.Discrete_Tests;
package body Util.Beans.Objects.Discretes is

   use Ada.Calendar;

   function "-" (Left, Right : Ada.Calendar.Time) return Ada.Calendar.Time;
   function "+" (Left, Right : Ada.Calendar.Time) return Ada.Calendar.Time;
   function Time_Value (S : String) return Ada.Calendar.Time;

   function "-" (Left, Right : Boolean) return Boolean;
   function "+" (Left, Right : Boolean) return Boolean;

   package Test_Integer is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Integer,
                                        To_Type        => Util.Beans.Objects.To_Integer,
                                        To_Object_Test => Util.Beans.Objects.To_Object,
                                        Value          => Integer'Value,
                                        Test_Name      => "Integer",
                                        Test_Values    => "-100,1,0,1,1000");

   package Test_Duration is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Duration,
                                        To_Type        => Util.Beans.Objects.To_Duration,
                                        To_Object_Test => Util.Beans.Objects.To_Object,
                                        Value          => Duration'Value,
                                        Test_Name      => "Duration",
                                        Test_Values    => "-100,1,0,1,1000");

   package Test_Long_Integer is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Long_Integer,
                                To_Type        => Util.Beans.Objects.To_Long_Integer,
                                To_Object_Test => Util.Beans.Objects.To_Object,
                                Value          => Long_Integer'Value,
                                Test_Name      => "Long_Integer",
                                Test_Values    => "-100,1,0,1,1000");

   package Test_Long_Long_Integer is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Long_Long_Integer,
                                To_Type        => Util.Beans.Objects.To_Long_Long_Integer,
                                To_Object_Test => Util.Beans.Objects.To_Object,
                                Value          => Long_Long_Integer'Value,
                                Test_Name      => "Long_Long_Integer",
                                Test_Values => "-10000000000000,1,0,1,1000_000_000_000");

   function "-" (Left, Right : Boolean) return Boolean is
   begin
      return Left and Right;
   end "-";

   function "+" (Left, Right : Boolean) return Boolean is
   begin
      return Left or Right;
   end "+";

   package Test_Boolean is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Boolean,
                                To_Type        => Util.Beans.Objects.To_Boolean,
                                To_Object_Test => Util.Beans.Objects.To_Object,
                                Value          => Boolean'Value,
                                Test_Name      => "Boolean",
                                Test_Values    => "false,true");

   type Color is (WHITE, BLACK, RED, GREEN, BLUE, YELLOW);

   package Color_Object is new Util.Beans.Objects.Enums (Color, ROUND_VALUE => True);
   function "-" (Left, Right : Color) return Color;
   function "+" (Left, Right : Color) return Color;

   function "-" (Left, Right : Color) return Color is
      N : constant Integer := Color'Pos (Left) - Color'Pos (Right);
   begin
      if N >= 0 then
         return Color'Val ((Color'Pos (WHITE) + N) mod 6);
      else
         return Color'Val ((Color'Pos (WHITE) - N) mod 6);
      end if;
   end "-";

   function "+" (Left, Right : Color) return Color is
      N : constant Integer := Color'Pos (Left) + Color'Pos (Right);
   begin
      return Color'Val ((Color'Pos (WHITE) + N) mod 6);
   end "+";

   package Test_Enum is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Color,
                                To_Type        => Color_Object.To_Value,
                                To_Object_Test => Color_Object.To_Object,
                                Value          => Color'Value,
                                Test_Name      => "Color",
                                Test_Values    => "BLACK,RED,GREEN,BLUE,YELLOW");

   Epoch : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Year    => Year_Number'First,
                           Month   => 1,
                           Day     => 1,
                           Seconds => 12 * 3600.0);

   function Time_Value (S : String) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Formatting.Value (S);
   end Time_Value;

   --  For the purpose of the time unit test, we need Time + Time operation even
   --  if this does not really makes sense.
   function "+" (Left, Right : Ada.Calendar.Time) return Ada.Calendar.Time is
      T1 : constant Duration := Left - Epoch;
      T2 : constant Duration := Right - Epoch;
   begin
      return (T1 + T2) + Epoch;
   end "+";

   function "-" (Left, Right : Ada.Calendar.Time) return Ada.Calendar.Time is
      T1 : constant Duration := Left - Epoch;
      T2 : constant Duration := Right - Epoch;
   begin
      return (T1 - T2) + Epoch;
   end "-";

   package Test_Time is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Ada.Calendar.Time,
                                To_Type        => Util.Beans.Objects.Time.To_Time,
                                To_Object_Test => Util.Beans.Objects.Time.To_Object,
                                Value          => Time_Value,
                                Test_Name      => "Time",
                                Test_Values => "1970-03-04 12:12:00,1975-05-04 13:13:10");

   package Test_Float is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Float,
                                To_Type        => Util.Beans.Objects.To_Float,
                                To_Object_Test => Util.Beans.Objects.To_Object,
                                Value          => Float'Value,
                                Test_Name      => "Float",
                                Test_Values    => "1.2,3.3,-3.3");

   package Test_Long_Float is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Long_Float,
                                To_Type        => Util.Beans.Objects.To_Long_Float,
                                To_Object_Test => Util.Beans.Objects.To_Object,
                                Value          => Long_Float'Value,
                                Test_Name      => "Long_Float",
                                Test_Values    => "1.2,3.3,-3.3");

   package Test_Long_Long_Float is new
     Util.Beans.Objects.Discrete_Tests (Test_Type      => Long_Long_Float,
                                To_Type        => Util.Beans.Objects.To_Long_Long_Float,
                                To_Object_Test => Util.Beans.Objects.To_Object,
                                Value          => Long_Long_Float'Value,
                                Test_Name      => "Long_Long_Float",
                                Test_Values    => "1.2,3.3,-3.3");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Test_Boolean.Add_Tests (Suite);
      Test_Integer.Add_Tests (Suite);
      Test_Long_Integer.Add_Tests (Suite);
      Test_Duration.Add_Tests (Suite);
      Test_Long_Long_Integer.Add_Tests (Suite);
      Test_Time.Add_Tests (Suite);
      Test_Float.Add_Tests (Suite);
      Test_Long_Float.Add_Tests (Suite);
      Test_Long_Long_Float.Add_Tests (Suite);
      Test_Enum.Add_Tests (Suite);
   end Add_Tests;

end Util.Beans.Objects.Discretes;
