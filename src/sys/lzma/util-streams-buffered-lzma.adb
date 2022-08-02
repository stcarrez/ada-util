-----------------------------------------------------------------------
--  util-streams-buffered-lzma -- LZMA streams
--  Copyright (C) 2018, 2019, 2022 Stephane Carrez
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
with Ada.IO_Exceptions;
package body Util.Streams.Buffered.Lzma is

   use type Interfaces.C.size_t;
   use type Base.lzma_ret;

   subtype size_t is Interfaces.C.size_t;
   subtype Offset is Ada.Streams.Stream_Element_Offset;

   --  -----------------------
   --  Initialize the stream to write on the given stream.
   --  An internal buffer is allocated for writing the stream.
   --  -----------------------
   overriding
   procedure Initialize (Stream  : in out Compress_Stream;
                         Output  : access Output_Stream'Class;
                         Size    : in Positive) is
      Result : Base.lzma_ret;
   begin
      Output_Buffer_Stream (Stream).Initialize (Output, Size);

      Stream.Stream.next_out := Stream.Buffer (Stream.Buffer'First)'Unchecked_Access;
      Stream.Stream.avail_out := Stream.Buffer'Length;
      Result := Container.lzma_easy_encoder (Stream.Stream'Unchecked_Access, 6,
                                             Check.LZMA_CHECK_CRC64);
      if Result /= Base.LZMA_OK then
         raise Ada.IO_Exceptions.Device_Error with "Cannot initialize compressor";
      end if;
   end Initialize;

   --  -----------------------
   --  Close the sink.
   --  -----------------------
   overriding
   procedure Close (Stream : in out Compress_Stream) is
   begin
      Stream.Flush;
      Output_Buffer_Stream (Stream).Close;
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
            Stream.Stream.avail_in := size_t (Buffer'Length);
            Encoded := True;
         end if;

         Result := Base.lzma_code (Stream.Stream'Unchecked_Access, Base.LZMA_RUN);

         --  Write the output data when the buffer is full or we reached the end of stream.
         if Stream.Stream.avail_out = 0 or else Result = Base.LZMA_STREAM_END then
            Last_Pos := Stream.Buffer'First + Stream.Buffer'Length
              - Offset (Stream.Stream.avail_out) - 1;
            Stream.Output.Write (Stream.Buffer (Stream.Buffer'First .. Last_Pos));
            Stream.Stream.next_out := Stream.Buffer (Stream.Buffer'First)'Unchecked_Access;
            Stream.Stream.avail_out := Stream.Buffer'Length;
         end if;
         exit when Result /= Base.LZMA_OK or else (Stream.Stream.avail_in = 0 and then Encoded);
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
      --  Stream is closed, ignore the flush.
      if Stream.Buffer = null then
         return;
      end if;
      Stream.Stream.next_in := null;
      Stream.Stream.avail_in := 0;
      loop
         Result := Base.lzma_code (Stream.Stream'Unchecked_Access, Base.LZMA_FINISH);

         if Stream.Stream.avail_out = 0 or else Result = Base.LZMA_STREAM_END then
            Last_Pos := Stream.Buffer'First + Stream.Buffer'Length
              - Offset (Stream.Stream.avail_out) - 1;
            if Last_Pos >= Stream.Buffer'First then
               Stream.Output.Write (Stream.Buffer (Stream.Buffer'First .. Last_Pos));
            end if;
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
      Object.Flush;
      Output_Buffer_Stream (Object).Finalize;
      Base.lzma_end (Object.Stream'Unchecked_Access);
   end Finalize;

   --  -----------------------
   --  Initialize the stream to write on the given stream.
   --  An internal buffer is allocated for writing the stream.
   --  -----------------------
   overriding
   procedure Initialize (Stream  : in out Decompress_Stream;
                         Input   : access Input_Stream'Class;
                         Size    : in Positive) is
      Result : Base.lzma_ret;
   begin
      Input_Buffer_Stream (Stream).Initialize (Input, Size);

      Stream.Stream.next_in := Stream.Buffer (Stream.Buffer'First)'Unchecked_Access;
      Stream.Stream.avail_in := 0;
      Result := Container.lzma_stream_decoder (Stream.Stream'Unchecked_Access,
                                               Long_Long_Integer'Last,
                                               Container.LZMA_CONCATENATED);
      if Result /= Base.LZMA_OK then
         raise Ada.IO_Exceptions.Device_Error with "Cannot initialize decompressor";
      end if;
   end Initialize;

   --  -----------------------
   --  Write the buffer array to the output stream.
   --  -----------------------
   overriding
   procedure Read (Stream : in out Decompress_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
      use type Base.lzma_action;

      Result : Base.lzma_ret;
   begin
      Stream.Stream.next_out  := Into (Into'First)'Unrestricted_Access;
      Stream.Stream.avail_out := Into'Length;
      loop
         if Stream.Stream.avail_in = 0
           and then not Stream.Is_Eof
           and then Stream.Action = Base.LZMA_RUN
         then
            Stream.Fill;
            if Stream.Write_Pos >= Stream.Read_Pos then
               Stream.Stream.avail_in := size_t (Stream.Write_Pos - Stream.Read_Pos);
               Stream.Stream.next_in := Stream.Buffer (Stream.Buffer'First)'Unchecked_Access;
            else
               Stream.Stream.avail_in := 0;
               Stream.Stream.next_in := null;
               Stream.Action := Base.LZMA_FINISH;
            end if;
         end if;
         Result := Base.lzma_code (Stream.Stream'Unchecked_Access, Stream.Action);

         if Stream.Stream.avail_out = 0 or else Result = Base.LZMA_STREAM_END then
            Last := Into'First + Into'Length
              - Offset (Stream.Stream.avail_out) - 1;
            return;
         end if;
         if Result /= Base.LZMA_OK then
            raise Ada.IO_Exceptions.Data_Error with "Decompression error";
         end if;
      end loop;
   end Read;

   --  -----------------------
   --  Flush the stream and release the buffer.
   --  -----------------------
   overriding
   procedure Finalize (Object : in out Decompress_Stream) is
   begin
      Base.lzma_end (Object.Stream'Unchecked_Access);
      Input_Buffer_Stream (Object).Finalize;
   end Finalize;

end Util.Streams.Buffered.Lzma;
