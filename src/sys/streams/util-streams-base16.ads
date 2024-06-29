-----------------------------------------------------------------------
--  util-streams-base16 -- Base16 encoding and decoding streams
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
