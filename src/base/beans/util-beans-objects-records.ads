-----------------------------------------------------------------------
--  util-beans-objects-records -- Generic Typed Data Representation
--  Copyright (C) 2011, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
