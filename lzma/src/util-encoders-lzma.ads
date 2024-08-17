-----------------------------------------------------------------------
--  util-encoders-lzma -- LZMA compression and decompression
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams;
with Ada.Finalization;
with Lzma.Base;
use Lzma;

package Util.Encoders.Lzma is

   --  ------------------------------
   --  Compress encoder
   --  ------------------------------
   --  This <b>Encoder</b> compresses the (binary) input stream using LZMA.
   type Compress is limited new Util.Encoders.Transformer with private;

   --  Compress the binary input stream represented by <b>Data</b> by using
   --  the LZMA compression into the output stream <b>Into</b>.
   --
   --  If the transformer does not have enough room to write the result,
   --  it must return in <b>Encoded</b> the index of the last encoded
   --  position in the <b>Data</b> stream.
   --
   --  The transformer returns in <b>Last</b> the last valid position
   --  in the output stream <b>Into</b>.
   --
   --  The <b>Encoding_Error</b> exception is raised if the input
   --  stream cannot be transformed.
   overriding
   procedure Transform (E       : in out Compress;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset);

   --  Finish compression of the input array.
   overriding
   procedure Finish (E    : in out Compress;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset);

private

   type Compress is limited new Ada.Finalization.Limited_Controlled
     and Util.Encoders.Transformer with record
      Stream : aliased Base.lzma_stream := Base.LZMA_STREAM_INIT;
   end record;

   overriding
   procedure Initialize (E : in out Compress);

   overriding
   procedure Finalize (E : in out Compress);

end Util.Encoders.Lzma;
