-----------------------------------------------------------------------
--  util-beans-objects-records -- Generic Typed Data Representation
--  Copyright (C) 2011, 2022 Stephane Carrez
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
   type Element_Type is private;
package Util.Beans.Objects.Records is

   type Element_Type_Access is access all Element_Type;

   --  Create an object which holds a record of the type <b>Element_Type</b>.
   function Create return Object;

   --  Create an object which is initialized with the given value.
   function To_Object (Value : in Element_Type) return Object;

   --  Returns the element
   function To_Element (Value : in Object) return Element_Type;

   --  Returns an access to the element.
   function To_Element_Access (Value : in Object) return Element_Type_Access;

private
   type Element_Proxy is new Proxy with record
      Value : aliased Element_Type;
   end record;
end Util.Beans.Objects.Records;
