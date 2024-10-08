-----------------------------------------------------------------------
--  util-strings -- Various String Utility
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2011, 2012, 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Finalization;
private with Util.Concurrent.Counters;
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

   --  Search for the first occurrence of the pattern in the string.
   function Index (Source  : in String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward) return Natural;

   --  Returns True if the source string starts with the given prefix.
   function Starts_With (Source  : in String;
                         Prefix  : in String) return Boolean;

   --  Returns True if the source string ends with the given suffix.
   function Ends_With (Source  : in String;
                       Suffix  : in String) return Boolean;

   --  Returns True if the source contains the pattern.
   function Contains (Source  : in String;
                      Pattern : in String) return Boolean;

   --  Simple string replacement within the source of the specified content
   --  by another string.  By default, replace only the first sequence.
   function Replace (Source  : in String;
                     Content : in String;
                     By      : in String;
                     First   : in Boolean := True) return String;

   --  Returns Integer'Image (Value) with the possible space stripped.
   function Image (Value : in Integer) return String;

   --  Returns Integer'Image (Value) with the possible space stripped.
   function Image (Value : in Long_Long_Integer) return String;

   package String_Access_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Name_Access,
      Element_Type    => Name_Access,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   package String_Set is new Ada.Containers.Hashed_Sets
     (Element_Type    => Name_Access,
      Hash            => Hash,
      Equivalent_Elements => Equivalent_Keys);

   --  String reference
   type String_Ref is private;

   --  Create a string reference from a string.
   function To_String_Ref (S : in String) return String_Ref;

   --  Create a string reference from an unbounded string.
   function To_String_Ref (S : in Ada.Strings.Unbounded.Unbounded_String) return String_Ref;

   --  Get the string
   function To_String (S : in String_Ref) return String;

   --  Get the string as an unbounded string
   function To_Unbounded_String (S : in String_Ref) return Ada.Strings.Unbounded.Unbounded_String;

   --  Compute the hash value of the string reference.
   function Hash (Key : in String_Ref) return Ada.Containers.Hash_Type;

   --  Returns true if left and right string references are equivalent.
   function Equivalent_Keys (Left, Right : in String_Ref) return Boolean;
   overriding
   function "=" (Left, Right : in String_Ref) return Boolean renames Equivalent_Keys;
   function "=" (Left  : in String_Ref;
                 Right : in String) return Boolean;
   function "=" (Left  : in String_Ref;
                 Right : in Ada.Strings.Unbounded.Unbounded_String) return Boolean;

   --  Returns the string length.
   function Length (S : in String_Ref) return Natural;

private
   pragma Inline (To_String_Ref);
   pragma Inline (To_String);

   type String_Record (Len : Natural) is limited record
      Counter : Util.Concurrent.Counters.Counter;
      Str     : String (1 .. Len);
   end record;
   type String_Record_Access is access all String_Record;

   type String_Ref is new Ada.Finalization.Controlled with record
      Str : String_Record_Access := null;
   end record;

   --  Increment the reference counter.
   overriding
   procedure Adjust (Object : in out String_Ref);

   --  Decrement the reference counter and free the allocated string.
   overriding
   procedure Finalize (Object : in out String_Ref);

end Util.Strings;
