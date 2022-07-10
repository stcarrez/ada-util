-----------------------------------------------------------------------
--  util-nullables -- Basic types that can hold a null value
--  Copyright (C) 2017, 2021, 2022 Stephane Carrez
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

   DEFAULT_TIME : constant Ada.Calendar.Time;

   --  ------------------------------
   --  A boolean which can be null.
   --  ------------------------------
   type Nullable_Boolean is record
      Value   : Boolean := False;
      Is_Null : Boolean := True;
   end record;

   Null_Boolean : constant Nullable_Boolean;

   --  Return True if the two nullable times are identical (both null or both same value).
   overriding
   function "=" (Left, Right : in Nullable_Boolean) return Boolean is
      ((Left.Is_Null = Right.Is_Null) and then (Left.Is_Null or else Left.Value = Right.Value));

   --  ------------------------------
   --  An integer which can be null.
   --  ------------------------------
   type Nullable_Integer is record
      Value   : Integer := 0;
      Is_Null : Boolean := True;
   end record;

   Null_Integer : constant Nullable_Integer;

   --  Return True if the two nullable times are identical (both null or both same value).
   overriding
   function "=" (Left, Right : in Nullable_Integer) return Boolean is
      ((Left.Is_Null = Right.Is_Null) and then (Left.Is_Null or else Left.Value = Right.Value));

   --  ------------------------------
   --  A long which can be null.
   --  ------------------------------
   type Nullable_Long is record
      Value   : Long_Long_Integer := 0;
      Is_Null : Boolean := True;
   end record;

   Null_Long : constant Nullable_Long;

   --  Return True if the two nullable times are identical (both null or both same value).
   overriding
   function "=" (Left, Right : in Nullable_Long) return Boolean is
      ((Left.Is_Null = Right.Is_Null) and then (Left.Is_Null or else Left.Value = Right.Value));

   --  ------------------------------
   --  A string which can be null.
   --  ------------------------------
   type Nullable_String is record
      Value   : Ada.Strings.Unbounded.Unbounded_String;
      Is_Null : Boolean := True;
   end record;

   Null_String : constant Nullable_String;

   --  Return True if the two nullable times are identical (both null or both same value).
   overriding
   function "=" (Left, Right : in Nullable_String) return Boolean is
      ((Left.Is_Null = Right.Is_Null) and then (Left.Is_Null or else Left.Value = Right.Value));

   --  ------------------------------
   --  A date which can be null.
   --  ------------------------------
   type Nullable_Time is record
      Value   : Ada.Calendar.Time := DEFAULT_TIME;
      Is_Null : Boolean := True;
   end record;

   Null_Time : constant Nullable_Time;

   --  Return True if the two nullable times are identical (both null or both same time).
   overriding
   function "=" (Left, Right : in Nullable_Time) return Boolean is
      ((Left.Is_Null = Right.Is_Null) and then (Left.Is_Null or else Left.Value = Right.Value));

private

   DEFAULT_TIME : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year    => 1901,
                                                                      Month   => 1,
                                                                      Day     => 2,
                                                                      Seconds => 0.0);
   Null_Boolean : constant Nullable_Boolean
     := Nullable_Boolean '(Is_Null => True,
                           Value   => False);

   Null_Integer : constant Nullable_Integer
     := Nullable_Integer '(Is_Null => True,
                           Value   => 0);

   Null_Long : constant Nullable_Long
     := Nullable_Long '(Is_Null => True,
                        Value   => 0);

   Null_String : constant Nullable_String
     := Nullable_String '(Is_Null => True,
                          Value   => Ada.Strings.Unbounded.Null_Unbounded_String);

   Null_Time : constant Nullable_Time
     := Nullable_Time '(Is_Null => True,
                        Value   => DEFAULT_TIME);

end Util.Nullables;
