-----------------------------------------------------------------------
--  Util-strings -- Various String Utility
--  Copyright (C) 2001, 2002, 2003, 2009, 2010 Stephane Carrez
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

with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
package Util.Strings is

   type String_Access is access all String;

   --  Constant string access
   type Name_Access is access constant String;

   --  Compute the hash value of the string.
   function Hash (Key : Name_Access) return Ada.Containers.Hash_Type;

   --  Returns true if left and right strings are equivalent.
   function Equivalent_Keys (Left, Right : Name_Access) return Boolean;

   package String_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Name_Access,
      Element_Type    => Name_Access,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

end Util.Strings;
