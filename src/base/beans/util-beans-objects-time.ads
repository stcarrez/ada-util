-----------------------------------------------------------------------
--  util-beans-objects-time  -- Helper conversion for Ada Calendar Time
--  Copyright (C) 2010, 2019 Stephane Carrez
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
