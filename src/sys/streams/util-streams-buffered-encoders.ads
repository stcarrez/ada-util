-----------------------------------------------------------------------
--  util-streams-encoders -- Streams with encoding and decoding capabilities
--  Copyright (C) 2017, 2019, 2021 Stephane Carrez
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

with Util.Encoders;

--  == Encoding Streams ==
--  The <tt>Encoding_Stream</tt> tagged record represents a stream with encoding capabilities.
--  The stream passes the data to be written to the <tt>Transformer</tt> interface that
--  allows to make transformations on the data before being written.
--
--     Encode : Util.Streams.Buffered.Encoders.Encoding_Stream;
--
--  The encoding stream manages a buffer that is used to hold the encoded data before it is
--  written to the target stream.  The <tt>Initialize</tt> procedure must be called to indicate
--  the target stream, the size of the buffer and the encoding format to be used.
--
--     Encode.Initialize (Output => File'Access, Size => 4096, Format => "base64");
--
generic
   type Encoder is limited new Util.Encoders.Transformer with private;
package Util.Streams.Buffered.Encoders is

   --  -----------------------
   --  Encoding stream
   --  -----------------------
   --  The <b>Encoding_Stream</b> is an output stream which uses an encoder to
   --  transform the data before writing it to the output.  The transformer can
   --  change the data by encoding it in Base64, Base16 or encrypting it.
   type Encoder_Stream is limited new Util.Streams.Buffered.Input_Output_Buffer_Stream
     with record
      Transform   : Encoder;
      Flushed     : Boolean := False;
   end record;

   --  Initialize the stream with a buffer of <b>Size</b> bytes.
   procedure Initialize (Stream  : in out Encoder_Stream;
                         Size    : in Positive);

   --  Initialize the stream to write on the given stream.
   --  An internal buffer is allocated for writing the stream.
   procedure Produces (Stream  : in out Encoder_Stream;
                       Output  : access Output_Stream'Class;
                       Size    : in Positive);

   --  Initialize the stream to read the given streams.
   procedure Consumes (Stream  : in out Encoder_Stream;
                       Input   : access Input_Stream'Class;
                       Size    : in Positive);

   --  Close the sink.
   overriding
   procedure Close (Stream : in out Encoder_Stream);

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Encoder_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Read into the buffer as many bytes as possible and return in
   --  `last` the position of the last byte read.
   overriding
   procedure Read (Stream : in out Encoder_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   overriding
   procedure Flush (Stream : in out Encoder_Stream);

   overriding
   procedure Finalize (Stream : in out Encoder_Stream);

   --  Fill the buffer by reading the input stream.
   --  Raises Data_Error if there is no input stream;
   procedure Fill (Stream : in out Encoder_Stream);

end Util.Streams.Buffered.Encoders;
