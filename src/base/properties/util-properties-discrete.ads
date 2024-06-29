-----------------------------------------------------------------------
--  util-properties-discrete -- Generic package for get/set of discrete properties
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

generic
   type Property_Type is (<>);
   --  with function To_String (Val : Property_Type) return String is <>;
   --  with function From_String (Val : String) return Property_Type is <>;
package Util.Properties.Discrete is

   --  Get the property value
   function Get (Self : in Manager'Class;
                 Name : in String) return Property_Type;

   --  Get the property value.
   --  Return the default if the property does not exist.
   function Get (Self    : in Manager'Class;
                 Name    : in String;
                 Default : in Property_Type) return Property_Type;

   --  Set the property value
   procedure Set (Self  : in out Manager'Class;
                  Name  : in String;
                  Value : in Property_Type);

end Util.Properties.Discrete;
