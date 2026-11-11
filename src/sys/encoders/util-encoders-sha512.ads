-----------------------------------------------------------------------
--  util-encoders-sha512 -- Compute SHA-512 hash
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams;
with GNAT.SHA512;

package Util.Encoders.SHA512 is

   HASH_SIZE : constant := 64;

   --  The SHA-512 binary hash (512-bit).
   subtype Hash_Array is Ada.Streams.Stream_Element_Array (0 .. HASH_SIZE - 1);

   --  The SHA-512 hash as hexadecimal string.
   subtype Digest is String (1 .. 2 * HASH_SIZE);

   subtype Base64_Digest is String (1 .. 88);

   --  ------------------------------
   --  SHA512 Context
   --  ------------------------------
   subtype Context is GNAT.SHA512.Context;

   --  Update the hash with the string.
   procedure Update (E : in out Context;
                     S : in String) renames GNAT.SHA512.Update;

   --  Update the hash with the string.
   procedure Update (E : in out Context;
                     S : in Ada.Streams.Stream_Element_Array) renames GNAT.SHA512.Update;

   --  Computes the SHA512 hash and returns the raw binary hash in <b>Hash</b>.
   procedure Finish (E    : in out Context;
                     Hash : out Hash_Array);

   --  Computes the SHA512 hash and returns the hexadecimal hash in <b>Hash</b>.
   procedure Finish (E    : in out Context;
                     Hash : out Digest);

   --  Computes the SHA512 hash and returns the base64 hash in <b>Hash</b>.
   procedure Finish_Base64 (E    : in out Context;
                            Hash : out Base64_Digest);

   --  ------------------------------
   --  SHA-512 encoder
   --  ------------------------------
   --  This <b>Encoder</b> translates the (binary) input stream into
   --  an SHA-512 hash stream.  The default output format is hexadecimal.
   type Encoder is new Util.Encoders.Transformer with private;
   type Encoder_Access is access all Encoder'Class;

   --  Format produced by the encoder.
   type Format_Type is (Binary, Base16, Base64, Base64URL);

   --  Set the output format of the SHA-512 encoder.
   procedure Set_Format (E      : in out Encoder;
                         Format : in Format_Type);

   --  Encodes the binary input stream represented by <b>Data</b> into
   --  an SHA-512 hash output stream <b>Into</b>.
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
   procedure Transform (E       : in out Encoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset);

   --  Finish the encoding: compute the SHA-512 hash of the data accumulated by
   --  the successive <b>Transform</b> calls and write the digest in <b>Into</b>
   --  using the output format set by <b>Set_Format</b>.
   overriding
   procedure Finish (E    : in out Encoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset);

private

   type Encoder is new Util.Encoders.Transformer with record
      --  The SHA-512 context which accumulates the data given to Transform.
      Ctx    : Context;
      Format : Format_Type := Base16;
   end record;

end Util.Encoders.SHA512;
