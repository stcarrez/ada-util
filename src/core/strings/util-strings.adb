-----------------------------------------------------------------------
--  util-strings -- Various String Utility
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2011, 2012, 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;
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
      if Left = null or else Right = null then
         return False;
      end if;
      return Left.all = Right.all;
   end Equivalent_Keys;

   --  ------------------------------
   --  Returns Integer'Image (Value) with the possible space stripped.
   --  ------------------------------
   function Image (Value : in Integer) return String is
      S : constant String := Integer'Image (Value);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Image;

   --  ------------------------------
   --  Returns Integer'Image (Value) with the possible space stripped.
   --  ------------------------------
   function Image (Value : in Long_Long_Integer) return String is
      S : constant String := Long_Long_Integer'Image (Value);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Image;

   use Util.Concurrent.Counters;

   --  ------------------------------
   --  Create a string reference from a string.
   --  ------------------------------
   function To_String_Ref (S : in String) return String_Ref is
      Str : constant String_Record_Access
        := new String_Record '(Len => S'Length, Str => S, Counter => ONE);
   begin
      return String_Ref '(Ada.Finalization.Controlled with
                          Str => Str);
   end To_String_Ref;

   --  ------------------------------
   --  Create a string reference from an unbounded string.
   --  ------------------------------
   function To_String_Ref (S : in Ada.Strings.Unbounded.Unbounded_String) return String_Ref is
      use Ada.Strings.Unbounded;

      Len : constant Natural := Length (S);
      Str : constant String_Record_Access
        := new String_Record '(Len => Len, Str => To_String (S), Counter => ONE);
   begin
      return String_Ref '(Ada.Finalization.Controlled with
                          Str => Str);
   end To_String_Ref;

   --  ------------------------------
   --  Get the string
   --  ------------------------------
   function To_String (S : in String_Ref) return String is
   begin
      if S.Str = null then
         return "";
      else
         return S.Str.Str;
      end if;
   end To_String;

   --  ------------------------------
   --  Get the string as an unbounded string
   --  ------------------------------
   function To_Unbounded_String (S : in String_Ref)
                                 return Ada.Strings.Unbounded.Unbounded_String is
   begin
      if S.Str = null then
         return Ada.Strings.Unbounded.Null_Unbounded_String;
      else
         return Ada.Strings.Unbounded.To_Unbounded_String (S.Str.Str);
      end if;
   end To_Unbounded_String;

   --  ------------------------------
   --  Compute the hash value of the string reference.
   --  ------------------------------
   function Hash (Key : String_Ref) return Ada.Containers.Hash_Type is
   begin
      if Key.Str = null then
         return 0;
      else
         return Ada.Strings.Hash (Key.Str.Str);
      end if;
   end Hash;

   --  ------------------------------
   --  Returns true if left and right string references are equivalent.
   --  ------------------------------
   function Equivalent_Keys (Left, Right : String_Ref) return Boolean is
   begin
      if Left.Str = Right.Str then
         return True;
      elsif Left.Str = null or else Right.Str = null then
         return False;
      else
         return Left.Str.Str = Right.Str.Str;
      end if;
   end Equivalent_Keys;

   function "=" (Left  : in String_Ref;
                 Right : in String) return Boolean is
   begin
      if Left.Str = null then
         return False;
      else
         return Left.Str.Str = Right;
      end if;
   end "=";

   function "=" (Left  : in String_Ref;
                 Right : in Ada.Strings.Unbounded.Unbounded_String) return Boolean is
      use Ada.Strings.Unbounded;
   begin
      if Left.Str = null then
         return Right = Null_Unbounded_String;
      else
         return Right = Left.Str.Str;
      end if;
   end "=";

   --  ------------------------------
   --  Returns the string length.
   --  ------------------------------
   function Length (S : in String_Ref) return Natural is
   begin
      if S.Str = null then
         return 0;
      else
         return S.Str.Len;
      end if;
   end Length;

   --  ------------------------------
   --  Increment the reference counter.
   --  ------------------------------
   overriding
   procedure Adjust (Object : in out String_Ref) is
   begin
      if Object.Str /= null then
         Util.Concurrent.Counters.Increment (Object.Str.Counter);
      end if;
   end Adjust;

   --  ------------------------------
   --  Decrement the reference counter and free the allocated string.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out String_Ref) is

      procedure Free is
        new Ada.Unchecked_Deallocation (String_Record, String_Record_Access);

      Is_Zero : Boolean;
   begin
      if Object.Str /= null then
         Util.Concurrent.Counters.Decrement (Object.Str.Counter, Is_Zero);
         if Is_Zero then
            Free (Object.Str);
         else
            Object.Str := null;
         end if;
      end if;
   end Finalize;

   --  ------------------------------
   --  Search for the first occurrence of the character in the string
   --  after the from index.  This implementation is 3-times faster than
   --  the Ada.Strings.Fixed version.
   --  Returns the index of the first occurrence or 0.
   --  ------------------------------
   function Index (Source : in String;
                   Char   : in Character;
                   From   : in Natural := 0) return Natural is
      Pos : Natural := From;
   begin
      if Pos < Source'First then
         Pos := Source'First;
      end if;
      for I in Pos .. Source'Last loop
         if Source (I) = Char then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   --  ------------------------------
   --  Search for the first occurrence of the pattern in the string.
   --  ------------------------------
   function Index (Source  : in String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward) return Natural is
   begin
      return Ada.Strings.Fixed.Index (Source, Pattern, From, Going);
   end Index;

   --  ------------------------------
   --  Returns True if the source string starts with the given prefix.
   --  ------------------------------
   function Starts_With (Source  : in String;
                         Prefix  : in String) return Boolean is
   begin
      return Source'Length >= Prefix'Length
        and then Source (Source'First .. Source'First + Prefix'Length - 1) = Prefix;
   end Starts_With;

   --  ------------------------------
   --  Returns True if the source string ends with the given suffix.
   --  ------------------------------
   function Ends_With (Source  : in String;
                       Suffix  : in String) return Boolean is
   begin
      return Source'Length >= Suffix'Length
        and then Source (Source'Last - Suffix'Length + 1 .. Source'Last) = Suffix;
   end Ends_With;

   --  ------------------------------
   --  Returns True if the source contains the pattern.
   --  ------------------------------
   function Contains (Source  : in String;
                      Pattern : in String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Source, Pattern) /= 0;
   end Contains;

   --  ------------------------------
   --  Search for the first occurrence of the character in the string
   --  before the from index and going backward.
   --  This implementation is 3-times faster than the Ada.Strings.Fixed version.
   --  Returns the index of the first occurrence or 0.
   --  ------------------------------
   function Rindex (Source : in String;
                    Ch     : in Character;
                    From   : in Natural := 0) return Natural is
      Pos : Natural := From;
   begin
      if Pos < Source'First then
         Pos := Source'Last;
      end if;
      for I in reverse Source'First .. Pos loop
         if Source (I) = Ch then
            return I;
         end if;
      end loop;
      return 0;
   end Rindex;

   --  ------------------------------
   --  Simple string replacement within the source of the specified content
   --  by another string.  By default, replace only the first sequence.
   --  ------------------------------
   function Replace (Source  : in String;
                     Content : in String;
                     By      : in String;
                     First   : in Boolean := True) return String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      Pos    : Natural := Source'First;
   begin
      while Pos <= Source'Last loop
         if Source'Last - Pos >= Content'Length
           and then Source (Pos .. Pos + Content'Length - 1) = Content
         then
            Append (Result, By);
            Pos := Pos + Content'Length;
            if First and then Pos <= Source'Last then
               Append (Result, Source (Pos .. Source'Last));
               Pos := Source'Last + 1;
            end if;
         else
            Append (Result, Source (Pos));
            Pos := Pos + 1;
         end if;
      end loop;
      return To_String (Result);
   end Replace;

end Util.Strings;
