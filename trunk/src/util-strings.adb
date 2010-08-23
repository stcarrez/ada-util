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

   package body Formats is
      type Code is mod 2**32;

      procedure Format (Content   : in Input;
                        Arguments : in Value_List;
                        Into      : in out Stream) is
         C : Code;
         Old_Pos : Natural;
         N       : Natural;
         Pos     : Natural := Content'First;
      begin
         while Pos <= Content'Last loop
            C := Char'Pos (Content (Pos));
            if C = Character'Pos ('{') then
               N := 0;
               Pos := Pos + 1;
               Old_Pos := Pos;
               while Pos <= Content'Last loop
                  C := Char'Pos (Content (Pos));
                  if C >= Character'Pos ('0') and C <= Character'Pos ('9') then
                     N := N * 10 + Natural (C - Character'Pos ('0'));
                     Pos := Pos + 1;
                  elsif C = Character'Pos ('}') then
                     if N > Arguments'Length then
                        Put (Into, '{');
                        Pos := Old_Pos;
                     else
                        Format (Arguments (N + Arguments'First), Into);
                        Pos := Pos + 1;
                     end if;
                     exit;
                  else
                     Put (Into, '{');
                     Pos := Old_Pos;
                     exit;
                  end if;
               end loop;
            else
               Put (Into, Character'Val (C));
               Pos := Pos + 1;
            end if;
         end loop;
      end Format;

      procedure Format (Argument : in Value;
                        Into     : in out Stream) is
         Content : constant Input := To_Input (Argument);
         C       : Code;
      begin
         for I in Content'Range loop
            C := Char'Pos (Content (I));
            Put (Into, Character'Val (C));
         end loop;
      end Format;

   end Formats;

end Util.Strings;
