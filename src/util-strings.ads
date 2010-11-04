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
with Ada.Containers.Indefinite_Hashed_Sets;
with Util.Concurrent.Counters;
package Util.Strings is

   pragma Preelaborate;

   --  Constant string access
   type Name_Access is access constant String;

   --  Compute the hash value of the string.
   function Hash (Key : Name_Access) return Ada.Containers.Hash_Type;

   --  Returns true if left and right strings are equivalent.
   function Equivalent_Keys (Left, Right : Name_Access) return Boolean;

   --  Search for the first occurrence of the character in the string
   --  after the from index.  This implementation is 3-times faster than
   --  the Ada.Strings.Fixed version.
   --  Returns the index of the first occurrence or 0.
   function Index (Source : in String;
                   Char   : in Character;
                   From   : in Natural := 0) return Natural;

   --  Search for the first occurrence of the character in the string
   --  before the from index and going backward.
   --  This implementation is 3-times faster than the Ada.Strings.Fixed version.
   --  Returns the index of the first occurrence or 0.
   function Rindex (Source : in String;
                    Ch     : in Character;
                    From   : in Natural := 0) return Natural;

   package String_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Name_Access,
      Element_Type    => Name_Access,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   package String_Set is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type    => Name_Access,
      Hash            => Hash,
      Equivalent_Elements => Equivalent_Keys);

   --  String reference
   type String_Ref is private;

   function To_String_Ref (S : in String) return String_Ref;

   function To_String (S : in String_Ref) return String;

private

   type String_Record (Len : Natural) is limited record
      Str     : String (1 .. Len);
      Counter : Util.Concurrent.Counters.Counter;
   end record;
   type String_Record_Access is access all String_Record;

   type String_Ref is new Ada.Finalization.Controlled with record
      Str : String_Record_Access := null;
   end record;

   overriding
   procedure Adjust (Object : in out String_Ref);

   overriding
   procedure Finalize (Object : in out String_Ref);

end Util.Strings;
