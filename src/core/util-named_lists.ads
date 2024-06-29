-----------------------------------------------------------------------
--  util-named_lists -- simple lists of elements with names
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
--  Used internally by the Util.Log.Appenders and Util.Log.Formatters.
--  Intended for these specific needs and be small.
generic
   type Element_Type is abstract tagged limited private;
package Util.Named_Lists is

   type Internal is limited private;

   type Named_Element (Length : Natural) is new Element_Type with record
      Next : Internal;
      Name : String (1 .. Length);
   end record;

   type Named_Element_Access is access all Named_Element'Class;

   type Named_List is limited private;

   function Find (List : in Named_List;
                  Name : in String) return Named_Element_Access;

   procedure Add (List : in out Named_List;
                  Element : in Named_Element_Access);

   procedure Clear (List : in out Named_List);

private

   type Internal is limited record
      Element : Named_Element_Access;
   end record;

   type Named_List is limited record
      First : Named_Element_Access;
   end record;

end Util.Named_Lists;
