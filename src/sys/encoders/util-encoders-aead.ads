-----------------------------------------------------------------------
--  util-encoders-aead -- ChaCha20-Poly1305 AEAD (RFC 8439)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Interfaces;
with Util.Encoders.ChaCha20;
private with Ada.Finalization;
private with Util.Encoders.Poly1305;

--  == AEAD ChaCha20-Poly1305 ==
--  The `Util.Encoders.AEAD` package implements the AEAD (Authenticated Encryption
--  with Associated Data) construction of RFC 8439 combining the ChaCha20 stream
--  cipher with the Poly1305 one-time authenticator:
--
--  * the one-time Poly1305 key is the first 32 bytes of the ChaCha20 keystream
--    block with counter 0,
--  * the message is encrypted with the keystream starting at counter 1,
--  * the 128-bit tag authenticates the optional AAD and the ciphertext:
--
--    <tt>AAD || pad16 || ciphertext || pad16 || u64le(len(AAD))
--                     || u64le(len(ciphertext))</tt>.
--
--  The `Encoder` and `Decoder` types implement the `Util.Encoders.Transformer`
--  interface (similar to AES and ChaCha20).  The `Encoder` streams the ciphertext
--  in `Transform` and appends the 16-byte tag in `Finish`.  The `Decoder` expects
--  the tag as the last 16 bytes of the input stream and verifies it in `Finish`;
--  it raises `Encoding_Error` when the tag is invalid.  The decrypted data written
--  by `Transform` must not be used before `Finish` has completed without error.
--
--  All the state is kept in the object itself: no buffer is dynamically allocated
--  and the data is copied only once from the input to the output array.
--
--    Enc : Util.Encoders.AEAD.Encoder;
--    ...
--    Enc.Set_Key (Key);
--    Enc.Set_Nonce (Nonce12);
--    Enc.Update_AAD (AAD);            --  optional, before Transform
--    Enc.Transform (Plain, Cipher, Last, Encoded);
--    Enc.Finish (Tag_Buf, Tag_Last);  --  16-byte tag
--
--  The nonce must be unique for each message encrypted with the same key.
package Util.Encoders.AEAD with Preelaborate is

   --  Number of bytes of the AEAD key.
   Key_Size : constant := Util.Encoders.ChaCha20.Key_Size;

   --  Number of bytes of the 96-bit nonce.
   Nonce_Size : constant := Util.Encoders.ChaCha20.Nonce_Size;

   --  Number of bytes of the Poly1305 authentication tag.
   Tag_Size : constant := 16;

   --  A 256-bit AEAD key.
   subtype Key is Util.Encoders.ChaCha20.Key;

   --  A 96-bit nonce.
   subtype Nonce is Util.Encoders.ChaCha20.Nonce;

   --  The 128-bit authentication tag.
   subtype Tag is Ada.Streams.Stream_Element_Array (1 .. Tag_Size);

   type Cipher is tagged limited private;

   --  Set the 256-bit AEAD key.
   procedure Set_Key (E    : in out Cipher;
                      Data : in Secret_Key) with
      Pre => Data.Length = Key_Size;

   --  Set the 96-bit nonce.  The nonce must be unique for each message
   --  encrypted with the same key.
   procedure Set_Nonce (E       : in out Cipher;
                        Nonce12 : in Nonce);

   --  Return true if the cipher has an AEAD key configured.
   function Has_Key (E : in Cipher) return Boolean;

   --  Return true if the cipher has a nonce configured.
   function Has_Nonce (E : in Cipher) return Boolean;

   --  Authenticate additional data (AAD).  Must be called before the first
   --  `Transform` call: the AAD is not encrypted but is covered by the tag.
   procedure Update_AAD (E    : in out Cipher;
                         Data : in     Ada.Streams.Stream_Element_Array) with
      Pre => E.Has_Key and E.Has_Nonce;

   --  ------------------------------
   --  AEAD encoder
   --  ------------------------------
   --  This <b>Encoder</b> encrypts the binary input stream with ChaCha20 and
   --  authenticates it with Poly1305.  `Transform` writes the ciphertext (same
   --  length as the input, no padding) and `Finish` writes the 16-byte tag.
   type Encoder is new Cipher and Util.Encoders.Transformer with private;

   Max_Finish_Size : constant := Tag_Size;

   --  Encrypt the binary input stream represented by <b>Data</b> into
   --  the output stream <b>Into</b>.  The output has the same length as
   --  the processed input.
   overriding
   procedure Transform (E       : in out Encoder;
                        Data    : in     Ada.Streams.Stream_Element_Array;
                        Into    : out    Ada.Streams.Stream_Element_Array;
                        Last    : out    Ada.Streams.Stream_Element_Offset;
                        Encoded : out    Ada.Streams.Stream_Element_Offset) with
      Pre => E.Has_Key and E.Has_Nonce;

   --  Finish the encryption: write the 16-byte authentication tag in <b>Into</b>
   --  and set <b>Last</b> to the last tag position.
   overriding
   procedure Finish (E    : in out Encoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset) with
      Pre => E.Has_Key and E.Has_Nonce and Into'Length >= Tag_Size;

   --  ------------------------------
   --  AEAD decoder
   --  ------------------------------
   --  This <b>Decoder</b> verifies and decrypts a binary input stream produced
   --  by the <b>Encoder</b>.  The input stream is the concatenation of the
   --  ciphertext and the 16-byte tag: `Transform` decrypts the ciphertext and
   --  holds back the last 16 bytes, and `Finish` verifies the tag.
   type Decoder is new Cipher and Util.Encoders.Transformer with private;

   --  Decrypt the binary input stream represented by <b>Data</b> into
   --  the output stream <b>Into</b>.  The last 16 bytes of the whole input
   --  stream are the authentication tag and are not decrypted.
   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in     Ada.Streams.Stream_Element_Array;
                        Into    : out    Ada.Streams.Stream_Element_Array;
                        Last    : out    Ada.Streams.Stream_Element_Offset;
                        Encoded : out    Ada.Streams.Stream_Element_Offset) with
      Pre => E.Has_Key and E.Has_Nonce;

   --  Verify the authentication tag (the last 16 bytes received by `Transform`).
   --  Raises <b>Encoding_Error</b> when the tag is missing or invalid.
   --  No output is produced.
   overriding
   procedure Finish (E    : in out Decoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset) with
      Pre => E.Has_Key and E.Has_Nonce;

private

   subtype Block_Type is Ada.Streams.Stream_Element_Array (1 .. Util.Encoders.ChaCha20.Block_Size);

   type Cipher is limited new Ada.Finalization.Limited_Controlled with record
      Cipher_Key    : Ada.Streams.Stream_Element_Array (1 .. Key_Size) := [others => 0];
      Cipher_Nonce  : Nonce := [others => 0];
      Counter       : Interfaces.Unsigned_32 := 0;
      Keystream     : Block_Type := [others => 0];
      Keystream_Pos : Ada.Streams.Stream_Element_Offset := Block_Type'Last + 1;
      Key_Set       : Boolean := False;
      Nonce_Set     : Boolean := False;
      Started       : Boolean := False;
      Mac           : Util.Encoders.Poly1305.Context;
      AAD_Len       : Interfaces.Unsigned_64 := 0;
      Data_Len      : Interfaces.Unsigned_64 := 0;
      AAD_Padded    : Boolean := False;
      Hold          : Tag := [others => 0];
      Hold_Len      : Natural := 0;
   end record;

   overriding
   procedure Finalize (Object : in out Cipher);

   type Encoder is new Cipher and Util.Encoders.Transformer with null record;
   type Decoder is new Cipher and Util.Encoders.Transformer with null record;

end Util.Encoders.AEAD;
