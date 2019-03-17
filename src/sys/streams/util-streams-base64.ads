-----------------------------------------------------------------------
--  util-streams-base64 -- Base64 encoding and decoding streams
--  Copyright (C) 2017, 2019 Stephane Carrez
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

with Util.Encoders.Base64;
with Util.Streams.Buffered.Encoders;

--  == Base64 Encoding Streams ==
--  The `Util.Streams.Base64` package provides streams to encode and decode the stream
--  using Base64.
package Util.Streams.Base64 is

   package Encoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.Base64.Encoder);

   package Decoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.Base64.Decoder);

   type Encoding_Stream is new Encoding.Encoder_Stream with null record;

   type Decoding_Stream is new Decoding.Encoder_Stream with null record;

end Util.Streams.Base64;
