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
      if Left = null or Right = null then
         return False;
      end if;
      return Left.all = Right.all;
   end Equivalent_Keys;


   use Util.Concurrent.Counters;
   function To_String_Ref (S : in String) return String_Ref is
      Str : constant String_Record_Access
        := new String_Record '(Len => S'Length, Str => S, Counter => ONE);
   begin
       return String_Ref '(Ada.Finalization.Controlled with
                           Str => Str);
   end To_String_Ref;

   function To_String (S : in String_Ref) return String is
   begin
      return S.Str.Str;
   end To_String;

   overriding
   procedure Adjust (Object : in out String_Ref) is
   begin
      if Object.Str /= null then
          Util.Concurrent.Counters.Increment (Object.Str.Counter);
      end if;
   end Adjust;

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
          end if;
      end if;
   end Finalize;

   package body Escape is
      type Code is mod 2**32;

      procedure Put_Hex (Into : in out Stream; Value : Code) is
         Conversion : constant String (1 .. 16) := "0123456789ABCDEF";
         S : String (1 .. 6) := (1 => '\', 2 => 'u', others => '0');
         P : Code := Value;
         N : Code;
         I : Positive := S'Last;
      begin
         while P /= 0 loop
            N := P mod 16;
            P := P / 16;
            S (I) := Conversion (Positive'Val (N + 1));
            exit when I = 1;
            I := I - 1;
         end loop;
         for J in S'Range loop
            Put (Into, S (J));
         end loop;
      end Put_Hex;

      procedure Escape_Java_Script (Content : in Input;
                                    Into    : in out Stream) is
      begin
         Escape_Java (Content => Content, Into => Into,
                      Escape_Single_Quote => True);
      end Escape_Java_Script;

      procedure Escape_Java (Content : in Input;
                             Into    : in out Stream) is
      begin
         Escape_Java (Content => Content, Into => Into,
                      Escape_Single_Quote => False);
      end Escape_Java;

      procedure Escape_Java (Content : in Input;
                             Escape_Single_Quote : in Boolean;
                             Into : in out Stream) is
         C : Code;
      begin
         for I in Content'Range loop
            C := Char'Pos (Content (I));
            if C < 16#20# then
               if C = 16#0A# then
                  Put (Into, '\');
                  Put (Into, 'n');

               elsif C = 16#0D# then
                  Put (Into, '\');
                  Put (Into, 'r');

               elsif C = 16#08# then
                  Put (Into, '\');
                  Put (Into, 'b');

               elsif C = 16#09# then
                  Put (Into, '\');
                  Put (Into, 't');

               elsif C = 16#0C# then
                  Put (Into, '\');
                  Put (Into, 'f');
               else
                  Put_Hex (Into, C);
               end if;

            elsif C = 16#27# then
               if Escape_Single_Quote then
                  Put (Into, '\');
               end if;
               Put (Into, Character'Val (C));

            elsif C = 16#22# then
               Put (Into, '\');
               Put (Into, Character'Val (C));

            elsif C = 16#5C# then
               Put (Into, '\');
               Put (Into, Character'Val (C));

            elsif C > 16#80# then
               Put_Hex (Into, C);

            else
               Put (Into, Character'Val (C));
            end if;
         end loop;
      end Escape_Java;
   end Escape;

end Util.Strings;
