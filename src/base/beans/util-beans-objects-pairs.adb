-----------------------------------------------------------------------
--  Util.Beans.Objects.Pairs -- Pairs of objects
--  Copyright (C) 2013, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Beans.Objects.Pairs is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Pair;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "key" or else Name = "first" then
         return From.First;
      elsif Name = "value" or else Name = "second" then
         return From.Second;
      else
         return Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Pair;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "key" or else Name = "first" then
         From.First := Value;
      elsif Name = "value" or else Name = "second" then
         From.Second := Value;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Return an object represented by the pair of two values.
   --  ------------------------------
   function To_Object (First, Second : in Object) return Object is
      Result : constant Pair_Access := new Pair;
   begin
      Result.First  := First;
      Result.Second := Second;
      return To_Object (Result.all'Access);
   end To_Object;

end Util.Beans.Objects.Pairs;
