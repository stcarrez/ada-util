-----------------------------------------------------------------------
--  util-streams-base16 -- Base16 encoding and decoding streams
--  Copyright (C) 2019 Stephane Carrez
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

with Util.Encoders.Base16;
with Util.Streams.Buffered.Encoders;

--  == Base16 Encoding Streams ==
--  The `Util.Streams.Base16` package provides streams to encode and decode the stream
--  using Base16.
package Util.Streams.Base16 is

   package Encoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.Base16.Encoder);

   package Decoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.Base16.Decoder);

   type Encoding_Stream is new Encoding.Encoder_Stream with null record;

   type Decoding_Stream is new Decoding.Encoder_Stream with null record;

end Util.Streams.Base16;
