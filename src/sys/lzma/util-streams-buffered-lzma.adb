-----------------------------------------------------------------------
--  util-streams-buffered-lzma -- LZMA streams
--  Copyright (C) 2018, 2019 Stephane Carrez
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
with Lzma.Check;
with Lzma.Container;
with Interfaces.C;
package body Util.Streams.Buffered.Lzma is

   use type Interfaces.C.size_t;
   use type Base.lzma_ret;

   subtype Offset is Ada.Streams.Stream_Element_Offset;

   --  -----------------------
   --  Initialize the stream to write on the given stream.
   --  An internal buffer is allocated for writing the stream.
   --  -----------------------
   overriding
   procedure Initialize (Stream  : in out Compress_Stream;
                         Output  : in Output_Stream_Access;
                         Size    : in Positive) is
      Result : Base.lzma_ret;
   begin
      Output_Buffer_Stream (Stream).Initialize (Output, Size);

      Stream.Stream.next_out := Stream.Buffer (Stream.Buffer'First)'Unchecked_Access;
      Stream.Stream.avail_out := Stream.Buffer'Length;
      Result := Container.lzma_easy_encoder (Stream.Stream'Unchecked_Access, 6,
                                             Check.LZMA_CHECK_CRC64);
   end Initialize;

   --  -----------------------
   --  Close the sink.
   --  -----------------------
   overriding
   procedure Close (Stream : in out Compress_Stream) is
   begin
      Stream.Flush;
      Stream.Output.Close;
   end Close;

   --  -----------------------
   --  Write the buffer array to the output stream.
   --  -----------------------
   overriding
   procedure Write (Stream : in out Compress_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
      Last_Pos  : Ada.Streams.Stream_Element_Offset;
      Encoded   : Boolean := False;
      Result    : Base.lzma_ret;
   begin
      loop
         if Stream.Stream.avail_in = 0 then
            Stream.Stream.next_in := Buffer (Buffer'First)'Unrestricted_Access;
            Stream.Stream.avail_in := Interfaces.C.size_t (Buffer'Length);
            Encoded := True;
         end if;

         Result := Base.lzma_code (Stream.Stream'Unchecked_Access, Base.LZMA_RUN);

         --  Write the output data when the buffer is full or we reached the end of stream.
         if Stream.Stream.avail_out = 0 or Result = Base.LZMA_STREAM_END then
            Last_Pos := Stream.Buffer'First + Stream.Buffer'Length
              - Offset (Stream.Stream.avail_out) - 1;
            Stream.Output.Write (Stream.Buffer (Stream.Buffer'First .. Last_Pos));
            Stream.Stream.next_out := Stream.Buffer (Stream.Buffer'First)'Unchecked_Access;
            Stream.Stream.avail_out := Stream.Buffer'Length;
         end if;
         exit when Result /= Base.LZMA_OK or (Stream.Stream.avail_in = 0 and Encoded);
      end loop;
   end Write;

   --  -----------------------
   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   --  -----------------------
   overriding
   procedure Flush (Stream : in out Compress_Stream) is
      Last_Pos : Ada.Streams.Stream_Element_Offset;
      Result : Base.lzma_ret;
   begin
      Stream.Stream.next_in := null;
      Stream.Stream.avail_in := 0;
      loop
         Result := Base.lzma_code (Stream.Stream'Unchecked_Access, Base.LZMA_FINISH);

         if Stream.Stream.avail_out = 0 or Result = Base.LZMA_STREAM_END then
            Last_Pos := Stream.Buffer'First + Stream.Buffer'Length
              - Offset (Stream.Stream.avail_out) - 1;
            Stream.Output.Write (Stream.Buffer (Stream.Buffer'First .. Last_Pos));
            Stream.Stream.next_out := Stream.Buffer (Stream.Buffer'First)'Unchecked_Access;
            Stream.Stream.avail_out := Stream.Buffer'Length;
         end if;
         exit when Result /= Base.LZMA_OK;
      end loop;
   end Flush;

   --  -----------------------
   --  Flush the stream and release the buffer.
   --  -----------------------
   overriding
   procedure Finalize (Object : in out Compress_Stream) is
   begin
      null;
   end Finalize;

end Util.Streams.Buffered.Lzma;
