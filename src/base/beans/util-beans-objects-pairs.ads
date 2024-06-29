-----------------------------------------------------------------------
--  Util.Beans.Objects.Pairs -- Pairs of objects
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;

package Util.Beans.Objects.Pairs is

   --  The <tt>Pair</tt> type is a bean that contains two values named first/key and
   --  second/value.
   type Pair is new Util.Beans.Basic.Bean with record
      First  : Object;
      Second : Object;
   end record;
   type Pair_Access is access all Pair'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Pair;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Pair;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Return an object represented by the pair of two values.
   function To_Object (First, Second : in Object) return Object;

end Util.Beans.Objects.Pairs;
