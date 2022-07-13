-----------------------------------------------------------------------
--  util-streams-buffered -- Buffered streams utilities
--  Copyright (C) 2010 - 2022 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
package body Util.Streams.Buffered is

   procedure Free_Buffer is
     new Ada.Unchecked_Deallocation (Object => Stream_Element_Array,
                                     Name   => Buffer_Access);

   --  ------------------------------
   --  Initialize the stream to read or write on the given streams.
   --  An internal buffer is allocated for writing the stream.
   --  ------------------------------
   procedure Initialize (Stream  : in out Buffer_Stream;
                         Size    : in Positive) is
   begin
      Free_Buffer (Stream.Buffer);
      Stream.Last      := Stream_Element_Offset (Size);
      Stream.Buffer    := new Stream_Element_Array (1 .. Stream.Last);
      Stream.Write_Pos := 1;
      Stream.Read_Pos  := 1;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream to read from the string.
   --  ------------------------------
   procedure Initialize (Stream  : in out Buffer_Stream;
                         Content : in String) is
   begin
      Free_Buffer (Stream.Buffer);
      Stream.Last      := Stream_Element_Offset (Content'Length);
      Stream.Buffer    := new Stream_Element_Array (1 .. Content'Length);
      Stream.Write_Pos := Stream.Last + 1;
      Stream.Read_Pos  := 1;
      for I in Content'Range loop
         Stream.Buffer (Stream_Element_Offset (I - Content'First + 1))
           := Character'Pos (Content (I));
      end loop;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream to read or write on the given streams.
   --  An internal buffer is allocated for writing the stream.
   --  ------------------------------
   procedure Initialize (Stream  : in out Output_Buffer_Stream;
                         Output  : access Output_Stream'Class;
                         Size    : in Positive) is
   begin
      Stream.Initialize (Size);
      Stream.Output    := Output;
      Stream.No_Flush  := False;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream to read from the string.
   --  ------------------------------
   overriding
   procedure Initialize (Stream  : in out Input_Buffer_Stream;
                         Content : in String) is
   begin
      Buffer_Stream (Stream).Initialize (Content);
      Stream.Input     := null;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream with a buffer of <b>Size</b> bytes.
   --  ------------------------------
   overriding
   procedure Initialize (Stream  : in out Output_Buffer_Stream;
                         Size    : in Positive) is
   begin
      Buffer_Stream (Stream).Initialize (Size => Size);
      Stream.Output := null;
      Stream.No_Flush := True;
      Stream.Read_Pos := 1;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream to read or write on the given streams.
   --  An internal buffer is allocated for writing the stream.
   --  ------------------------------
   procedure Initialize (Stream  : in out Input_Buffer_Stream;
                         Input   : access Input_Stream'Class;
                         Size    : in Positive) is
   begin
      Free_Buffer (Stream.Buffer);
      Stream.Last      := Stream_Element_Offset (Size);
      Stream.Buffer    := new Stream_Element_Array (1 .. Stream.Last);
      Stream.Input     := Input;
      Stream.Write_Pos := 1;
      Stream.Read_Pos  := 1;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream from the buffer created for an output stream.
   --  ------------------------------
   procedure Initialize (Stream  : in out Input_Buffer_Stream;
                         From    : in out Buffer_Stream'Class) is
   begin
      Free_Buffer (Stream.Buffer);
      Stream.Buffer := From.Buffer;
      From.Buffer := null;
      Stream.Input := null;
      Stream.Read_Pos := 1;
      Stream.Write_Pos := From.Write_Pos + 1;
      Stream.Last := From.Last;
   end Initialize;

   --  ------------------------------
   --  Close the sink.
   --  ------------------------------
   overriding
   procedure Close (Stream : in out Output_Buffer_Stream) is
   begin
      if Stream.Output /= null then
         Output_Buffer_Stream'Class (Stream).Flush;
         Stream.Output.Close;
         Free_Buffer (Stream.Buffer);
      end if;
   end Close;

   --  ------------------------------
   --  Get the direct access to the buffer.
   --  ------------------------------
   function Get_Buffer (Stream : in Buffer_Stream) return Buffer_Access is
   begin
      return Stream.Buffer;
   end Get_Buffer;

   --  ------------------------------
   --  Get the number of element in the stream.
   --  ------------------------------
   function Get_Size (Stream : in Buffer_Stream) return Natural is
   begin
      return Natural (Stream.Write_Pos - Stream.Read_Pos);
   end Get_Size;

   --  ------------------------------
   --  Write the buffer array to the output stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out Output_Buffer_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
      Start : Stream_Element_Offset := Buffer'First;
      Pos   : Stream_Element_Offset := Stream.Write_Pos;
      Avail : Stream_Element_Offset;
      Size  : Stream_Element_Offset;
   begin
      while Start <= Buffer'Last loop
         Size := Buffer'Last - Start + 1;
         Avail := Stream.Last - Pos + 1;
         if Avail = 0 then
            if Stream.Output = null then
               raise Ada.IO_Exceptions.End_Error with "Buffer is full";
            end if;
            Stream.Output.Write (Stream.Buffer (1 .. Pos - 1));
            Stream.Write_Pos := 1;
            Pos := 1;
            Avail := Stream.Last - Pos + 1;
         end if;
         if Avail < Size then
            Size := Avail;
         end if;
         Stream.Buffer (Pos .. Pos + Size - 1) := Buffer (Start .. Start + Size - 1);
         Start := Start + Size;
         Pos   := Pos + Size;
         Stream.Write_Pos := Pos;

         --  If we have still more data than the buffer size, flush and write
         --  the buffer directly.
         if Start < Buffer'Last and then Buffer'Last - Start > Stream.Buffer'Length then
            if Stream.Output = null then
               raise Ada.IO_Exceptions.End_Error with "Buffer is full";
            end if;
            Stream.Output.Write (Stream.Buffer (1 .. Pos - 1));
            Stream.Write_Pos := 1;
            Stream.Output.Write (Buffer (Start .. Buffer'Last));
            return;
         end if;
      end loop;
   end Write;

   --  ------------------------------
   --  Flush the stream.
   --  ------------------------------
   overriding
   procedure Flush (Stream : in out Output_Buffer_Stream) is
   begin
      if not Stream.No_Flush then
         if Stream.Write_Pos > 1 then
            if Stream.Output /= null then
               Stream.Output.Write (Stream.Buffer (1 .. Stream.Write_Pos - 1));
            end if;
            Stream.Write_Pos := 1;
         end if;
         if Stream.Output /= null then
            Stream.Output.Flush;
         end if;
      end if;
   end Flush;

   --  ------------------------------
   --  Flush the buffer in the `Into` array and return the index of the
   --  last element (inclusive) in `Last`.
   --  ------------------------------
   procedure Flush (Stream : in out Output_Buffer_Stream;
                    Into   : out Ada.Streams.Stream_Element_Array;
                    Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      if Stream.Write_Pos > 1 then
         Into (Into'First .. Into'First + Stream.Write_Pos - 1) :=
           Stream.Buffer (Stream.Buffer'First .. Stream.Write_Pos - 1);
         Stream.Write_Pos := 1;
         Last := Into'First + Stream.Write_Pos - 1;
      else
         Last := Into'First - 1;
      end if;
   end Flush;

   --  ------------------------------
   --  Flush the buffer stream to the unbounded string.
   --  ------------------------------
   procedure Flush (Stream : in out Output_Buffer_Stream;
                    Into   : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Ada.Strings.Unbounded.Set_Unbounded_String (Into, "");
      if Stream.Write_Pos > 1 then
         for I in 1 .. Stream.Write_Pos - 1 loop
            Ada.Strings.Unbounded.Append (Into, Character'Val (Stream.Buffer (I)));
         end loop;
         Stream.Write_Pos := 1;
      end if;
   end Flush;

   --  ------------------------------
   --  Fill the buffer by reading the input stream.
   --  Raises Data_Error if there is no input stream;
   --  ------------------------------
   procedure Fill (Stream : in out Input_Buffer_Stream) is
   begin
      if Stream.Input = null then
         Stream.Eof := True;
      else
         Stream.Input.Read (Stream.Buffer (1 .. Stream.Last - 1), Stream.Write_Pos);
         Stream.Eof := Stream.Write_Pos < 1;
         if not Stream.Eof then
            Stream.Write_Pos := Stream.Write_Pos + 1;
         end if;
         Stream.Read_Pos := 1;
      end if;
   end Fill;

   --  ------------------------------
   --  Read one character from the input stream.
   --  ------------------------------
   procedure Read (Stream : in out Input_Buffer_Stream;
                   Char   : out Character) is
   begin
      if Stream.Read_Pos >= Stream.Write_Pos then
         Stream.Fill;
         if Stream.Eof then
            raise Ada.IO_Exceptions.Data_Error with "End of buffer";
         end if;
      end if;
      Char := Character'Val (Stream.Buffer (Stream.Read_Pos));
      Stream.Read_Pos := Stream.Read_Pos + 1;
   end Read;

   procedure Read (Stream : in out Input_Buffer_Stream;
                   Value  : out Ada.Streams.Stream_Element) is
   begin
      if Stream.Read_Pos >= Stream.Write_Pos then
         Stream.Fill;
         if Stream.Eof then
            raise Ada.IO_Exceptions.Data_Error with "End of buffer";
         end if;
      end if;
      Value := Stream.Buffer (Stream.Read_Pos);
      Stream.Read_Pos := Stream.Read_Pos + 1;
   end Read;

   --  ------------------------------
   --  Read one character from the input stream.
   --  ------------------------------
   procedure Read (Stream : in out Input_Buffer_Stream;
                   Char   : out Wide_Wide_Character) is
      use Interfaces;

      Val    : Ada.Streams.Stream_Element;
      Result : Unsigned_32;
   begin
      Stream.Read (Val);

      --  UTF-8 conversion
      --  7  U+0000   U+007F   1  0xxxxxxx
      if Val <= 16#7F# then
         Char := Wide_Wide_Character'Val (Val);

      --  11 U+0080   U+07FF   2  110xxxxx 10xxxxxx
      elsif Val <= 16#DF# then
         Result := Shift_Left (Unsigned_32 (Val and 16#1F#), 6);
         Stream.Read (Val);
         Result := Result or Unsigned_32 (Val and 16#3F#);
         Char := Wide_Wide_Character'Val (Result);

      --  16 U+0800   U+FFFF   3  1110xxxx 10xxxxxx 10xxxxxx
      elsif Val <= 16#EF# then
         Result := Shift_Left (Unsigned_32 (Val and 16#0F#), 12);
         Stream.Read (Val);
         Result := Result or Shift_Left (Unsigned_32 (Val and 16#3F#), 6);
         Stream.Read (Val);
         Result := Result or Unsigned_32 (Val and 16#3F#);
         Char := Wide_Wide_Character'Val (Result);

      --  21 U+10000  U+1FFFFF 4  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      else
         Result := Shift_Left (Unsigned_32 (Val and 16#07#), 18);
         Stream.Read (Val);
         Result := Result or Shift_Left (Unsigned_32 (Val and 16#3F#), 12);
         Stream.Read (Val);
         Result := Result or Shift_Left (Unsigned_32 (Val and 16#3F#), 6);
         Stream.Read (Val);
         Result := Result or Unsigned_32 (Val and 16#3F#);
         Char := Wide_Wide_Character'Val (Result);
      end if;
   end Read;

   --  ------------------------------
   --  Read into the buffer as many bytes as possible and return in
   --  `last` the position of the last byte read.
   --  ------------------------------
   overriding
   procedure Read (Stream : in out Input_Buffer_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
      Start : Stream_Element_Offset := Into'First;
      Pos   : Stream_Element_Offset := Stream.Read_Pos;
      Avail : Stream_Element_Offset;
      Size  : Stream_Element_Offset;
      Total : Stream_Element_Offset := 0;
   begin
      while Start <= Into'Last loop
         Size := Into'Last - Start + 1;
         Avail := Stream.Write_Pos - Pos;
         if Avail = 0 then
            Stream.Fill;
            Pos := Stream.Read_Pos;
            Avail := Stream.Write_Pos - Pos;
            exit when Avail <= 0;
         end if;
         if Avail < Size then
            Size := Avail;
         end if;
         Into (Start .. Start + Size - 1) := Stream.Buffer (Pos .. Pos + Size - 1);
         Start := Start + Size;
         Pos   := Pos + Size;
         Total := Total + Size;
         Stream.Read_Pos := Pos;
      end loop;
      Last := Total;
   end Read;

   --  ------------------------------
   --  Read into the buffer as many bytes as possible and return in
   --  `last` the position of the last byte read.
   --  ------------------------------
   procedure Read (Stream : in out Input_Buffer_Stream;
                   Into   : in out Ada.Strings.Unbounded.Unbounded_String) is
      Pos   : Stream_Element_Offset := Stream.Read_Pos;
      Avail : Stream_Element_Offset;
   begin
      loop
         Avail := Stream.Write_Pos - Pos;
         if Avail = 0 then
            Stream.Fill;
            if Stream.Eof then
               return;
            end if;
            Pos   := Stream.Read_Pos;
            Avail := Stream.Write_Pos - Pos;
         end if;
         for I in 1 .. Avail loop
            Ada.Strings.Unbounded.Append (Into, Character'Val (Stream.Buffer (Pos)));
            Pos := Pos + 1;
         end loop;
         Stream.Read_Pos := Pos;
      end loop;
   end Read;

   procedure Read (Stream : in out Input_Buffer_Stream;
                   Into   : in out Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
      Pos   : Stream_Element_Offset;
      Avail : Stream_Element_Offset;
      C     : Wide_Wide_Character;
   begin
      loop
         Pos := Stream.Read_Pos;
         Avail := Stream.Write_Pos - Pos;
         if Avail = 0 then
            Stream.Fill;
            if Stream.Eof then
               return;
            end if;
            Pos   := Stream.Read_Pos;
            Avail := Stream.Write_Pos - Pos;
         end if;
         Stream.Read (C);
         Ada.Strings.Wide_Wide_Unbounded.Append (Into, C);
      end loop;
   end Read;

   --  ------------------------------
   --  Flush the stream and release the buffer.
   --  ------------------------------
   overriding
   procedure Finalize (Stream : in out Output_Buffer_Stream) is
   begin
      if Stream.Buffer /= null then
         if Stream.Output /= null then
            Stream.Flush;
         end if;
         Free_Buffer (Stream.Buffer);
      end if;
   end Finalize;

   --  ------------------------------
   --  Returns True if the end of the stream is reached.
   --  ------------------------------
   function Is_Eof (Stream : in Input_Buffer_Stream) return Boolean is
   begin
      return Stream.Eof;
   end Is_Eof;

   --  ------------------------------
   --  Flush the stream and release the buffer.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Buffer_Stream) is
   begin
      if Object.Buffer /= null then
         Free_Buffer (Object.Buffer);
      end if;
   end Finalize;

end Util.Streams.Buffered;
