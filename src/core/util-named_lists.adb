-----------------------------------------------------------------------
--  util-named_lists -- simple lists of elements with names
--  Copyright (C) 2024 Stephane Carrez
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
