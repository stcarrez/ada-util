-----------------------------------------------------------------------
--  Util.Beans.Objects.Enums -- Helper conversion for discrete types
--  Copyright (C) 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

generic
   type T is (<>);

   --  When True, round the integer value held by the object before
   --  converting it into the type T.
   ROUND_VALUE : Boolean := False;
package Util.Beans.Objects.Enums is

   --  Create an object from the given value.
   function To_Object (Value : in T) return Object;

   --  Convert the object into a value.
   --  Raises Constraint_Error if the object cannot be converter to the target type.
   function To_Value (Value : in Util.Beans.Objects.Object) return T;

end Util.Beans.Objects.Enums;
