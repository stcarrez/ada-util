-----------------------------------------------------------------------
--  util-dates-to_ada_time_32 -- Convert Ada date to nanoseconds (old gnat)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
pragma Warning_As_Error ("*obsolecent*");

with Ada.Calendar.Conversions;

separate (Util.Dates)

function To_Ada_Time (Value : Nanosecond_Type) return Ada.Calendar.Time is
   use Ada.Calendar;
   Sec  : constant Interfaces.C.long :=
     Interfaces.C.long (Value / 1_000_000_000);
   Nsec : constant Nanosecond_Type := Value mod 1_000_000_000;
   D    : constant Duration := Conversions.To_Duration (0, Interfaces.C.long (Nsec));
begin
   return Conversions.To_Ada_Time (Sec) + D;
end To_Ada_Time;
