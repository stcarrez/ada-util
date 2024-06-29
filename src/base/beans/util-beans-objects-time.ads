-----------------------------------------------------------------------
--  util-beans-objects-time  -- Helper conversion for Ada Calendar Time
--  Copyright (C) 2010, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Calendar;
with Util.Nullables;
package Util.Beans.Objects.Time is

   --  Create an object from the given value.
   function To_Object (Value : in Ada.Calendar.Time) return Object;

   function To_Object (Value : in Nullables.Nullable_Time) return Object;

   --  Convert the object into a time.
   --  Raises Constraint_Error if the object cannot be converter to the target type.
   function To_Time (Value : in Object) return Ada.Calendar.Time;

   function To_Time (Value : in Object) return Nullables.Nullable_Time;

   --  Force the object to be a time.
   function Cast_Time (Value : Object) return Object;

end Util.Beans.Objects.Time;
