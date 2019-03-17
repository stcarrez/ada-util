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
with Lzma.Base;
use Lzma;

package Util.Streams.Buffered.Lzma is

   --  -----------------------
   --  Compress stream
   --  -----------------------
   --  The <b>Compress_Stream</b> is an output stream which uses the LZMA encoder to
   --  compress the data before writing it to the output.
   type Compress_Stream is limited new Util.Streams.Buffered.Output_Buffer_Stream with private;

   --  Initialize the stream to write on the given stream.
   --  An internal buffer is allocated for writing the stream.
   overriding
   procedure Initialize (Stream  : in out Compress_Stream;
                         Output  : access Output_Stream'Class;
                         Size    : in Positive);

   --  Close the sink.
   overriding
   procedure Close (Stream : in out Compress_Stream);

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Compress_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   overriding
   procedure Flush (Stream : in out Compress_Stream);

   --  -----------------------
   --  Compress stream
   --  -----------------------
   --  The <b>Compress_Stream</b> is an output stream which uses the LZMA encoder to
   --  compress the data before writing it to the output.
   type Decompress_Stream is limited new Util.Streams.Buffered.Input_Buffer_Stream with private;

   --  Initialize the stream to write on the given stream.
   --  An internal buffer is allocated for writing the stream.
   overriding
   procedure Initialize (Stream  : in out Decompress_Stream;
                         Input   : access Input_Stream'Class;
                         Size    : in Positive);

   --  Write the buffer array to the output stream.
   overriding
   procedure Read (Stream : in out Decompress_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

private

   type Compress_Stream is limited new Util.Streams.Buffered.Output_Buffer_Stream with record
      Stream : aliased Base.lzma_stream := Base.LZMA_STREAM_INIT;
   end record;

   --  Flush the stream and release the buffer.
   overriding
   procedure Finalize (Object : in out Compress_Stream);

   type Decompress_Stream is limited new Util.Streams.Buffered.Input_Buffer_Stream with record
      Stream : aliased Base.lzma_stream := Base.LZMA_STREAM_INIT;
      Action : Base.lzma_action := Base.LZMA_RUN;
   end record;

   --  Flush the stream and release the buffer.
   overriding
   procedure Finalize (Object : in out Decompress_Stream);

end Util.Streams.Buffered.Lzma;
