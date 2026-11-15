-----------------------------------------------------------------------
--  util-streams-base64 -- Base64 encoding and decoding streams
--  Copyright (C) 2017, 2019, 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Encoders.Base64;
with Util.Streams.Buffered.Encoders;

--  == Base64 Encoding Streams ==
--  The `Util.Streams.Base64` package provides streams to encode and decode the stream
--  using Base64.
package Util.Streams.Base64 is

   package Encoding is
     new Util.Streams.Buffered.Encoders
       (Encoder         => Util.Encoders.Base64.Encoder,
        Max_Finish_Size => Util.Encoders.Base64.Max_Finish_Size);

   package Decoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.Base64.Decoder);

   type Encoding_Stream is new Encoding.Encoder_Stream with null record;

   type Decoding_Stream is new Decoding.Encoder_Stream with null record;

end Util.Streams.Base64;
