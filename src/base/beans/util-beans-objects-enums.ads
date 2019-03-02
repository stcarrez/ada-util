-----------------------------------------------------------------------
--  Util.Beans.Objects.Enums -- Helper conversion for discrete types
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
