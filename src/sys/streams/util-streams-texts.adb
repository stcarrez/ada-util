-----------------------------------------------------------------------
--  util-streams-texts -- Text stream utilities
--  Copyright (C) 2010, 2011, 2012, 2016, 2017, 2019, 2020, 2021, 2022 Stephane Carrez
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
with Ada.IO_Exceptions;
package body Util.Streams.Texts is

   use Ada.Streams;

   procedure Initialize (Stream : in out Print_Stream;
                         To     : access Output_Stream'Class) is
   begin
      Stream.Initialize (Output => To, Size => 4096);
   end Initialize;

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String) is
      Count : constant Natural := Ada.Strings.Unbounded.Length (Item);
   begin
      if Count > 0 then
         for I in 1 .. Count loop
            Stream.Write (Item => Ada.Strings.Unbounded.Element (Item, I));
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
            Stream.Write_Wide (C);
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
      if Item >= 0 then
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
      if Item >= 0 then
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
                         From   : access Input_Stream'Class) is
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
         elsif C /= ASCII.CR or else not Strip then
            Ada.Strings.Unbounded.Append (Into, C);
         end if;
      end loop;

   exception
      when Ada.IO_Exceptions.Data_Error
        | Ada.IO_Exceptions.Device_Error
        | Ada.IO_Exceptions.End_Error =>
         return;
   end Read_Line;

end Util.Streams.Texts;
