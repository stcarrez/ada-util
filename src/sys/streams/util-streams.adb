-----------------------------------------------------------------------
--  util-streams -- Stream utilities
--  Copyright (C) 2010, 2011, 2016, 2018, 2020, 2021 Stephane Carrez
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

with Interfaces;
package body Util.Streams is

   use Ada.Streams;
   subtype Offset is Ada.Streams.Stream_Element_Offset;

   --  ------------------------------
   --  Copy the input stream to the output stream until the end of the input stream
   --  is reached.
   --  ------------------------------
   procedure Copy (From : in out Input_Stream'Class;
                   Into : in out Output_Stream'Class) is
      Buffer : Stream_Element_Array (0 .. 4_096);
      Last   : Stream_Element_Offset;
   begin
      loop
         From.Read (Buffer, Last);
         if Last > Buffer'First then
            Into.Write (Buffer (Buffer'First .. Last));
         end if;
         exit when Last < Buffer'Last;
      end loop;
   end Copy;

   --  ------------------------------
   --  Copy the stream array to the string.
   --  The string must be large enough to hold the stream array
   --  or a Constraint_Error exception is raised.
   --  ------------------------------
   procedure Copy (From : in Ada.Streams.Stream_Element_Array;
                   Into : in out String) is
      Target : Stream_Element_Array
        (Stream_Element_Offset (Into'First) .. Stream_Element_Offset (Into'Last));
      for Target'Address use Into'Address;
   begin
      Target (Target'First .. Target'First + From'Length - 1) := From;
   end Copy;

   --  ------------------------------
   --  Copy the string to the stream array.
   --  The stream array must be large enough to hold the string
   --  or a Constraint_Error exception is raised.
   --  ------------------------------
   procedure Copy (From : in String;
                   Into : in out Ada.Streams.Stream_Element_Array) is
      Target : String (Natural (Into'First) .. Natural (Into'Last));
      for Target'Address use Into'Address;
   begin
      Target (Target'First .. Target'First + From'Length - 1) := From;
   end Copy;

   --  ------------------------------
   --  Write a raw character on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Output_Stream'Class;
                    Item   : in Character) is
      Buf : constant Ada.Streams.Stream_Element_Array (1 .. 1)
        := (1 => Ada.Streams.Stream_Element (Character'Pos (Item)));
   begin
      Stream.Write (Buf);
   end Write;

   --  ------------------------------
   --  Write a wide character on the stream doing some conversion if necessary.
   --  The default implementation translates the wide character to a UTF-8 sequence.
   --  ------------------------------
   procedure Write_Wide (Stream : in out Output_Stream'Class;
                         Item   : in Wide_Wide_Character) is
      use Interfaces;

      Val : Unsigned_32;
      Buf : Ada.Streams.Stream_Element_Array (1 .. 4);
   begin
      --  UTF-8 conversion
      --  7  U+0000   U+007F   1  0xxxxxxx
      --  11 U+0080   U+07FF   2  110xxxxx 10xxxxxx
      --  16 U+0800   U+FFFF   3  1110xxxx 10xxxxxx 10xxxxxx
      --  21 U+10000  U+1FFFFF 4  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      Val := Wide_Wide_Character'Pos (Item);
      if Val <= 16#7f# then
         Buf (1) := Ada.Streams.Stream_Element (Val);
         Stream.Write (Buf (1 .. 1));
      elsif Val <= 16#07FF# then
         Buf (1) := Stream_Element (16#C0# or Shift_Right (Val, 6));
         Buf (2) := Stream_Element (16#80# or (Val and 16#03F#));
         Stream.Write (Buf (1 .. 2));
      elsif Val <= 16#0FFFF# then
         Buf (1) := Stream_Element (16#E0# or Shift_Right (Val, 12));
         Val := Val and 16#0FFF#;
         Buf (2) := Stream_Element (16#80# or Shift_Right (Val, 6));
         Buf (3) := Stream_Element (16#80# or (Val and 16#03F#));
         Stream.Write (Buf (1 .. 3));
      else
         Val := Val and 16#1FFFFF#;
         Buf (1) := Stream_Element (16#F0# or Shift_Right (Val, 18));
         Val := Val and 16#3FFFF#;
         Buf (2) := Stream_Element (16#80# or Shift_Right (Val, 12));
         Val := Val and 16#0FFF#;
         Buf (3) := Stream_Element (16#80# or Shift_Right (Val, 6));
         Buf (4) := Stream_Element (16#80# or (Val and 16#03F#));
         Stream.Write (Buf (1 .. 4));
      end if;
   end Write_Wide;

   procedure Write_Wide (Stream : in out Output_Stream'Class;
                         Item   : in Wide_Wide_String) is
   begin
      for C of Item loop
         Stream.Write_Wide (C);
      end loop;
   end Write_Wide;

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Output_Stream'Class;
                    Item   : in String) is
      Buf : Ada.Streams.Stream_Element_Array (Offset (Item'First) .. Offset (Item'Last));
      for Buf'Address use Item'Address;
   begin
      Stream.Write (Buf);
   end Write;

end Util.Streams;
