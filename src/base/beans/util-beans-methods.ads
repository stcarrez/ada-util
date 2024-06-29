-----------------------------------------------------------------------
--  util-beans-methods -- Bean methods
--  Copyright (C) 2010, 2011, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Strings;
package Util.Beans.Methods is

   pragma Preelaborate;

   type Method_Binding is tagged limited record
      Name : Util.Strings.Name_Access;
   end record;
   type Method_Binding_Access is access constant Method_Binding'Class;

   type Method_Binding_Array is array (Natural range <>) of Method_Binding_Access;
   type Method_Binding_Array_Access is access constant Method_Binding_Array;

   type Method_Bean is limited interface;
   type Method_Bean_Access is access all Method_Bean'Class;

   function Get_Method_Bindings (From : in Method_Bean)
                                 return Method_Binding_Array_Access is abstract;

end Util.Beans.Methods;
