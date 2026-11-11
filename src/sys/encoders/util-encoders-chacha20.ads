-----------------------------------------------------------------------
--  util-encoders-chacha20 -- ChaCha20 stream cipher (RFC 8439)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Interfaces;
private with Ada.Finalization;

--  == ChaCha20 stream cipher ==
--  The `Util.Encoders.ChaCha20` package implements the ChaCha20 stream cipher
--  defined by RFC 8439.  The cipher uses a 256-bit key, a 96-bit nonce and a
--  32-bit block counter.  The `Block` procedure produces the 64-byte
--  keystream block and the `Crypt` procedure applies the keystream on a
--  buffer.
--
--  The `Encoder` and `Decoder` types implement the `Util.Encoders.Transformer`
--  interface (similar to AES) so that a content can be encrypted or decrypted
--  by calling the `Transform` procedure.  ChaCha20 is a stream cipher: the
--  encryption and the decryption are the same operation (XOR with the
--  keystream) and the output has the same length as the input (no padding).
--  The key is configured with the `Set_Key` procedure and the nonce with the
--  `Set_Nonce` procedure.  The nonce must be unique for each message encrypted
--  with the same key.
--
--    Cipher : Util.Encoders.ChaCha20.Encoder;
--    ...
--    Cipher.Set_Key (Key);
--    Cipher.Set_Nonce (Nonce12);
--    Cipher.Transform (Data, Encrypted, Last, Encoded);
package Util.Encoders.ChaCha20 with Preelaborate is

   --  Number of bytes of the ChaCha20 key.
   Key_Size : constant := 32;

   --  Number of bytes of the 96-bit nonce.
   Nonce_Size : constant := 12;

   --  Number of bytes of a ChaCha20 keystream block.
   Block_Size : constant := 64;

   --  A 256-bit ChaCha20 key.
   subtype Key is Util.Encoders.Secret_Key (Length => Key_Size);

   --  A 96-bit ChaCha20 nonce.
   subtype Nonce is Ada.Streams.Stream_Element_Array (1 .. Nonce_Size);

   --  The ChaCha20 block function: produce the 64-byte keystream block for
   --  the given key, 32-bit block counter and 96-bit nonce (RFC 8439).
   procedure Block (Key32   : in Key;
                    Counter : in Interfaces.Unsigned_32;
                    Nonce12 : in Nonce;
                    Block   : out Ada.Streams.Stream_Element_Array) with
      Pre => Block'Length = Block_Size;

   --  XOR the data with the ChaCha20 keystream starting at First_Counter.
   --  Output must have the same length as Data.
   procedure Crypt (Key32         : in Key;
                    Nonce12       : in Nonce;
                    First_Counter : in Interfaces.Unsigned_32;
                    Data          : in Ada.Streams.Stream_Element_Array;
                    Output        : out Ada.Streams.Stream_Element_Array) with
      Pre => Output'Length = Data'Length;

   type Cipher is tagged limited private;

   --  Set the 256-bit encryption key.
   procedure Set_Key (E    : in out Cipher;
                      Data : in Secret_Key) with
      Pre => Data.Length = Key_Size;

   --  Set the 96-bit nonce.  The nonce must be unique for each message
   --  encrypted with the same key.
   procedure Set_Nonce (E       : in out Cipher;
                        Nonce12 : in Nonce);

   --  Set the initial 32-bit block counter (RFC 8439 uses 0 or 1).
   procedure Set_Counter (E       : in out Cipher;
                          Counter : in Interfaces.Unsigned_32);

   --  Return true if the cipher has an encryption key configured.
   function Has_Key (E : in Cipher) return Boolean;

   --  ------------------------------
   --  ChaCha20 encoder
   --  ------------------------------
   --  This <b>Encoder</b> encrypts the binary input stream by XOR-ing it with
   --  the ChaCha20 keystream.  As ChaCha20 is a stream cipher, the output has
   --  the same length as the input and no padding is added.
   type Encoder is new Cipher and Util.Encoders.Transformer with private;

   --  Encrypt the binary input stream represented by <b>Data</b> into
   --  the output stream <b>Into</b>.
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
                        Encoded : out Ada.Streams.Stream_Element_Offset) with
      Pre => E.Has_Key;

   --  ------------------------------
   --  ChaCha20 decoder
   --  ------------------------------
   --  This <b>Decoder</b> decrypts the binary input stream.  As ChaCha20 is
   --  a stream cipher, the decryption is the same operation as the encryption.
   type Decoder is new Cipher and Util.Encoders.Transformer with private;

   --  Decrypt the binary input stream represented by <b>Data</b> into
   --  the output stream <b>Into</b>.
   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) with
      Pre => E.Has_Key;

private

   subtype Block_Type is Ada.Streams.Stream_Element_Array (1 .. Block_Size);

   type Cipher is limited new Ada.Finalization.Limited_Controlled with record
      Cipher_Key    : Key;
      Cipher_Nonce  : Nonce := [others => 0];
      Counter       : Interfaces.Unsigned_32 := 0;
      Keystream     : Block_Type := [others => 0];
      Keystream_Pos : Ada.Streams.Stream_Element_Offset := Block_Size + 1;
      Key_Set       : Boolean := False;
   end record;

   overriding
   procedure Finalize (Object : in out Cipher);

   type Encoder is new Cipher and Util.Encoders.Transformer with null record;
   type Decoder is new Cipher and Util.Encoders.Transformer with null record;

end Util.Encoders.ChaCha20;
