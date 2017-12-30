-----------------------------------------------------------------------
--  util-streams-texts -- Text stream utilities
--  Copyright (C) 2010, 2011, 2012, 2016, 2017 Stephane Carrez
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
with Ada.IO_Exceptions;
package body Util.Streams.Texts is

   use Ada.Streams;
   subtype Offset is Ada.Streams.Stream_Element_Offset;

   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Output_Stream_Access) is
   begin
      Stream.Initialize (Output => To, Size => 4096);
   end Initialize;

   --  ------------------------------
   --  Write a raw character on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Char   : in Character) is
      Buf : constant Ada.Streams.Stream_Element_Array (1 .. 1)
        := (1 => Ada.Streams.Stream_Element (Character'Pos (Char)));
   begin
      Stream.Write (Buf);
   end Write;

   --  ------------------------------
   --  Write a wide character on the stream doing some conversion if necessary.
   --  The default implementation translates the wide character to a UTF-8 sequence.
   --  ------------------------------
   procedure Write_Wide (Stream : in out Print_Stream;
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

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in String) is
      Buf : Ada.Streams.Stream_Element_Array (Offset (Item'First) .. Offset (Item'Last));
      for Buf'Address use Item'Address;
   begin
      Stream.Write (Buf);
   end Write;

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String) is
      Count : constant Natural := Ada.Strings.Unbounded.Length (Item);
   begin
      if Count > 0 then
         for I in 1 .. Count loop
            Stream.Write (Char => Ada.Strings.Unbounded.Element (Item, I));
         end loop;
      end if;
   end Write;

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
      Count : constant Natural := Ada.Strings.Wide_Wide_Unbounded.Length (Item);
      C     : Wide_Wide_Character;
   begin
      if Count > 0 then
         for I in 1 .. Count loop
            C := Ada.Strings.Wide_Wide_Unbounded.Element (Item, I);
            Stream.Write (Char => Character'Val (Wide_Wide_Character'Pos (C)));
         end loop;
      end if;
   end Write;

   --  ------------------------------
   --  Write an integer on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Integer) is
      S : constant String := Integer'Image (Item);
   begin
      if Item > 0 then
         Stream.Write (S (S'First + 1 .. S'Last));
      else
         Stream.Write (S);
      end if;
   end Write;

   --  ------------------------------
   --  Write an integer on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Long_Long_Integer) is
      S : constant String := Long_Long_Integer'Image (Item);
   begin
      if Item > 0 then
         Stream.Write (S (S'First + 1 .. S'Last));
      else
         Stream.Write (S);
      end if;
   end Write;

   --  ------------------------------
   --  Write a date on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Calendar.Time;
                    Format : in GNAT.Calendar.Time_IO.Picture_String
                    := GNAT.Calendar.Time_IO.ISO_Date) is
   begin
      Stream.Write (GNAT.Calendar.Time_IO.Image (Item, Format));
   end Write;

   --  ------------------------------
   --  Get the output stream content as a string.
   --  ------------------------------
   function To_String (Stream : in Buffered.Output_Buffer_Stream'Class) return String is
      Size   : constant Natural := Stream.Get_Size;
      Buffer : constant Streams.Buffered.Buffer_Access := Stream.Get_Buffer;
      Result : String (1 .. Size);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Buffer (Stream_Element_Offset (I)));
      end loop;
      return Result;
   end To_String;

   --  ------------------------------
   --  Write a character on the stream.
   --  ------------------------------
   procedure Write_Char (Stream : in out Print_Stream'Class;
                         Item   : in Character) is
   begin
      Stream.Write (Item);
   end Write_Char;

   --  ------------------------------
   --  Write a character on the stream.
   --  ------------------------------
   procedure Write_Char (Stream : in out Print_Stream'Class;
                         Item   : in Wide_Wide_Character) is
   begin
      Stream.Write_Wide (Item);
   end Write_Char;

   --  ------------------------------
   --  Initialize the reader to read the input from the input stream given in <b>From</b>.
   --  ------------------------------
   procedure Initialize (Stream : in out Reader_Stream;
                         From   : in Input_Stream_Access) is
   begin
      Stream.Initialize (Input => From, Size => 4096);
   end Initialize;

   --  ------------------------------
   --  Read an input line from the input stream.  The line is terminated by ASCII.LF.
   --  When <b>Strip</b> is set, the line terminators (ASCII.CR, ASCII.LF) are removed.
   --  ------------------------------
   procedure Read_Line (Stream : in out Reader_Stream;
                        Into   : out Ada.Strings.Unbounded.Unbounded_String;
                        Strip  : in Boolean := False) is
      C : Character;
   begin
      while not Stream.Is_Eof loop
         Stream.Read (C);
         if C = ASCII.LF then
            if not Strip then
               Ada.Strings.Unbounded.Append (Into, C);
            end if;
            return;
         elsif C /= ASCII.CR or not Strip then
            Ada.Strings.Unbounded.Append (Into, C);
         end if;
      end loop;

   exception
      when Ada.IO_Exceptions.Data_Error =>
         return;
   end Read_Line;

end Util.Streams.Texts;
