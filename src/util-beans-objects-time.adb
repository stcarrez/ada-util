-----------------------------------------------------------------------
--  Util.Beans.Objects.Time -- Helper conversion for Ada Calendar Time
--  Copyright (C) 2010 Stephane Carrez
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

package body Util.Beans.Objects.Time is

   use Ada.Calendar;

   Epoch : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Year    => Year_Number'First,
                           Month   => 1,
                           Day     => 1,
                           Seconds => 12 * 3600.0);

   --  ------------------------------
   --  Time Type
   --  ------------------------------
   type Time_Type_Def is new Duration_Type_Def with null record;

   --  Get the type name
   function Get_Name (Type_Def : Time_Type_Def) return String;

   --  Convert the value into a string.
   function To_String (Type_Def : in Time_Type_Def;
                       Value    : in Object_Value) return String;

   Time_Type  : aliased constant Time_Type_Def := Time_Type_Def '(others => <>);

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   function Get_Name (Type_Def : in Time_Type_Def) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Time";
   end Get_Name;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   function To_String (Type_Def : in Time_Type_Def;
                       Value    : in Object_Value) return String is
      pragma Unreferenced (Type_Def);
   begin
      return Ada.Calendar.Formatting.Image (Epoch + Value.Time_Value);
   end To_String;

   --  ------------------------------
   --  Create an object from the given value.
   --  ------------------------------
   function To_Object (Value : in Ada.Calendar.Time) return Object is
   begin
      return Object '(Controlled with
                      V         => Object_Value '(Of_Type    => TYPE_TIME,
                                                  Time_Value => Value - Epoch),
                      Type_Def  => Time_Type'Access);
   end To_Object;

   --  ------------------------------
   --  Convert the object into a time.
   --  Raises Constraint_Error if the object cannot be converter to the target type.
   --  ------------------------------
   function To_Time (Value : in Object) return Ada.Calendar.Time is
   begin
      case Value.V.Of_Type is
         when TYPE_TIME =>
            return Value.V.Time_Value + Epoch;

         when TYPE_STRING | TYPE_WIDE_STRING =>
            return Ada.Calendar.Formatting.Value (Value.Type_Def.To_String (Value.V));

         when others =>
            raise Constraint_Error with "Conversion to a date is not possible";
      end case;
   end To_Time;

   --  ------------------------------
   --  Force the object to be a time.
   --  ------------------------------
   function Cast_Time (Value : Object) return Object is
   begin
      case Value.V.Of_Type is
         when TYPE_TIME =>
            return Value;

         when TYPE_STRING | TYPE_WIDE_STRING =>
            return Util.Beans.Objects.Time.To_Object (Ada.Calendar.Formatting.Value (Value.Type_Def.To_String (Value.V)));

         when others =>
            raise Constraint_Error with "Conversion to a date is not possible";
      end case;
   end Cast_Time;

end Util.Beans.Objects.Time;
