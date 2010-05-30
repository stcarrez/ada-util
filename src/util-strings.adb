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

with Ada.Strings.Hash;
package body Util.Strings is

   --  ------------------------------
   --  Compute the hash value of the string.
   --  ------------------------------
   function Hash (Key : Name_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Key.all);
   end Hash;

   --  ------------------------------
   --  Returns true if left and right strings are equivalent.
   --  ------------------------------
   function Equivalent_Keys (Left, Right : Name_Access) return Boolean is
   begin
      if Left = null or Right = null then
         return False;
      end if;
      return Left.all = Right.all;
   end Equivalent_Keys;

end Util.Strings;
