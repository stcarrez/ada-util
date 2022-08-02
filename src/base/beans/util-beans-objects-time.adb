-----------------------------------------------------------------------
--  util-beans-objects-time  -- Helper conversion for Ada Calendar Time
--  Copyright (C) 2010, 2013, 2016, 2019, 2022 Stephane Carrez
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

with Interfaces.C;
with Ada.Calendar.Formatting;
with Ada.Calendar.Conversions;

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
   overriding
   function Get_Name (Type_Def : Time_Type_Def) return String;

   --  Convert the value into a string.
   overriding
   function To_String (Type_Def : in Time_Type_Def;
                       Value    : in Object_Value) return String;

   Time_Type  : aliased constant Time_Type_Def := Time_Type_Def '(null record);

   --  ------------------------------
   --  Get the type name
   --  ------------------------------
   overriding
   function Get_Name (Type_Def : in Time_Type_Def) return String is
      pragma Unreferenced (Type_Def);
   begin
      return "Time";
   end Get_Name;

   --  ------------------------------
   --  Convert the value into a string.
   --  ------------------------------
   overriding
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

   function To_Object (Value : in Nullables.Nullable_Time) return Object is
   begin
      if Value.Is_Null then
         return Null_Object;
      else
         return To_Object (Value.Value);
      end if;
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
            declare
               T : constant String := Value.Type_Def.To_String (Value.V);
            begin
               return Ada.Calendar.Formatting.Value (T);

            exception
                  --  Last chance, try to convert a Unix time displayed as an integer.
               when Constraint_Error =>
                  return Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long'Value (T));

            end;

         when others =>
            raise Constraint_Error with "Conversion to a date is not possible";
      end case;
   end To_Time;

   function To_Time (Value : in Object) return Nullables.Nullable_Time is
   begin
      if Is_Null (Value) then
         return Nullables.Nullable_Time '(Is_Null => True, Value => Epoch);
      else
         return Nullables.Nullable_Time '(Is_Null => False, Value => To_Time (Value));
      end if;
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
            return Time.To_Object (Formatting.Value (Value.Type_Def.To_String (Value.V)));

         when others =>
            raise Constraint_Error with "Conversion to a date is not possible";
      end case;
   end Cast_Time;

end Util.Beans.Objects.Time;
