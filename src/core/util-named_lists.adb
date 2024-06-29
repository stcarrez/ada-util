-----------------------------------------------------------------------
--  util-named_lists -- simple lists of elements with names
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;
package body Util.Named_Lists is

   function Find (List : in Named_List;
                  Name : in String) return Named_Element_Access is
      Element : Named_Element_Access := List.First;
   begin
      while Element /= null loop
         if Element.Name = Name then
            return Element;
         end if;
         Element := Element.Next.Element;
      end loop;
      return null;
   end Find;

   procedure Add (List    : in out Named_List;
                  Element : in Named_Element_Access) is
   begin
      Element.Next.Element := List.First;
      List.First := Element;
   end Add;

   procedure Clear (List : in out Named_List) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => Named_Element'Class,
                                                        Name   => Named_Element_Access);
   begin
      loop
         declare
            Element : Named_Element_Access := List.First;
         begin
            exit when Element = null;
            List.First := Element.Next.Element;
            Free (Element);
         end;
      end loop;
   end Clear;

end Util.Named_Lists;
