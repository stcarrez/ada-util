-----------------------------------------------------------------------
--  util-nullables -- Basic types that can hold a null value
--  Copyright (C) 2017 Stephane Carrez
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
with Ada.Strings.Unbounded;

--  === Nullable types ===
--  Sometimes it is necessary to represent a simple data type with an optional boolean information
--  that indicates whether the value is valid or just null.  The concept of nullable type is often
--  used in databases but also in JSON data representation.  The <tt>Util.Nullables</tt> package
--  provides several standard type to express the null capability of a value.
--
--  By default a nullable instance is created with the null flag set.
package Util.Nullables is

   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Ada.Calendar.Time;

   --  ------------------------------
   --  A boolean which can be null.
   --  ------------------------------
   type Nullable_Boolean is record
      Value   : Boolean := False;
      Is_Null : Boolean := True;
   end record;

   --  Return True if the two nullable times are identical (both null or both same value).
   function "=" (Left, Right : in Nullable_Boolean) return Boolean is
      (Left.Is_Null = Right.Is_Null and (Left.Is_Null or else Left.Value = Right.Value));

   --  ------------------------------
   --  An integer which can be null.
   --  ------------------------------
   type Nullable_Integer is record
      Value   : Integer := 0;
      Is_Null : Boolean := True;
   end record;

   --  Return True if the two nullable times are identical (both null or both same value).
   function "=" (Left, Right : in Nullable_Integer) return Boolean is
      (Left.Is_Null = Right.Is_Null and (Left.Is_Null or else Left.Value = Right.Value));

   --  ------------------------------
   --  A long which can be null.
   --  ------------------------------
   type Nullable_Long is record
      Value   : Long_Long_Integer := 0;
      Is_Null : Boolean := True;
   end record;

   --  Return True if the two nullable times are identical (both null or both same value).
   function "=" (Left, Right : in Nullable_Long) return Boolean is
      (Left.Is_Null = Right.Is_Null and (Left.Is_Null or else Left.Value = Right.Value));

   --  ------------------------------
   --  A string which can be null.
   --  ------------------------------
   type Nullable_String is record
      Value   : Ada.Strings.Unbounded.Unbounded_String;
      Is_Null : Boolean := True;
   end record;

   --  Return True if the two nullable times are identical (both null or both same value).
   function "=" (Left, Right : in Nullable_String) return Boolean is
      (Left.Is_Null = Right.Is_Null and (Left.Is_Null or else Left.Value = Right.Value));

   --  ------------------------------
   --  A date which can be null.
   --  ------------------------------
   type Nullable_Time is record
      Value   : Ada.Calendar.Time;
      Is_Null : Boolean := True;
   end record;

   --  Return True if the two nullable times are identical (both null or both same time).
   function "=" (Left, Right : in Nullable_Time) return Boolean is
      (Left.Is_Null = Right.Is_Null and (Left.Is_Null or else Left.Value = Right.Value));

end Util.Nullables;
