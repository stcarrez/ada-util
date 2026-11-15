-----------------------------------------------------------------------
--  util-streams-aead -- AEAD (ChaCha20-Poly1305) encoding and decoding streams
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Util.Encoders;
with Util.Encoders.AEAD;
with Util.Streams.Buffered.Encoders;

--  == AEAD Encoding Streams ==
--  The `Util.Streams.AEAD` package defines the `Encoding_Stream` and
--  `Decoding_Stream` types to encrypt and decrypt a stream by using the
--  AEAD ChaCha20-Poly1305 construction (RFC 8439) provided by the
--  `Util.Encoders.AEAD` package.  Before using these streams, you must use
--  the `Set_Key` procedure to setup the 256-bit encryption key and the
--  `Set_Nonce` procedure to define the 96-bit nonce.  The nonce must be
--  unique for each message encrypted with the same key.
--
--  The encryption and decryption keys are represented by the
--  `Util.Encoders.Secret_Key` limited type.  The key must be 32 bytes
--  (`Util.Encoders.AEAD.Key_Size`).
--
--  Optional additional authenticated data (AAD) can be covered by the
--  authentication tag by using the `Update_AAD` procedure.  It must be
--  called before the first data is written (or read when decoding).
--
--  The `Encoding_Stream` writes the ciphertext (same length as the input)
--  and appends the 16-byte authentication tag when the stream is flushed
--  or closed.  The `Decoding_Stream` expects the input stream to be the
--  concatenation of the ciphertext and the 16-byte tag: it decrypts the
--  data and verifies the tag when the end of the input stream is reached.
--  It raises `Util.Encoders.Encoding_Error` when the tag is invalid, in
--  which case the decrypted data must not be used.
--
--  To write a text, encrypt the content and save the file, we can chain several
--  stream objects together.  Because they are chained, the last stream object in
--  the chain must be declared first and the first element of the chain will be
--  declared last.  The following declaration is used:
--
--      Out_Stream   : aliased Util.Streams.Files.File_Stream;
--      Cipher       : aliased Util.Streams.AEAD.Encoding_Stream;
--      Printer      : Util.Streams.Texts.Print_Stream;
--
--      Out_Stream.Initialize (Mode => Ada.Streams.Stream_IO.Out_File,
--                             Name => "encrypted.aead");
--      Cipher.Initialize (Output => Out_Stream'Unchecked_Access,
--                         Size   => 32768);
--      Printer.Initialize (Output => Cipher'Unchecked_Access,
--                          Size   => 4096);
--      Cipher.Set_Key (Key);
--      Cipher.Set_Nonce (Nonce12);
--      Cipher.Update_AAD (AAD);
--
--  It is now possible to write the text by using the `Printer` object:
--
--    Printer.Write ("Hello world!");
--
package Util.Streams.AEAD is

   package Encoding is
     new Util.Streams.Buffered.Encoders
       (Encoder         => Util.Encoders.AEAD.Encoder,
        Max_Finish_Size => Util.Encoders.AEAD.Max_Finish_Size);

   package Decoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.AEAD.Decoder);

   type Encoding_Stream is new Encoding.Encoder_Stream with null record;

   --  Set the 256-bit encryption key to be used.
   procedure Set_Key (Stream : in out Encoding_Stream;
                      Secret : in Util.Encoders.Secret_Key);

   --  Set the 96-bit nonce before starting the encryption.
   --  The nonce must be unique for each message encrypted with the same key.
   procedure Set_Nonce (Stream  : in out Encoding_Stream;
                        Nonce12 : in Util.Encoders.AEAD.Nonce);

   --  Authenticate additional data (AAD) before writing the first data.
   --  The AAD is not encrypted but is covered by the authentication tag.
   procedure Update_AAD (Stream : in out Encoding_Stream;
                         Data   : in Ada.Streams.Stream_Element_Array);

   type Decoding_Stream is new Decoding.Encoder_Stream with null record;

   --  Set the 256-bit decryption key to be used.
   procedure Set_Key (Stream : in out Decoding_Stream;
                      Secret : in Util.Encoders.Secret_Key);

   --  Set the 96-bit nonce before starting the decryption.
   procedure Set_Nonce (Stream  : in out Decoding_Stream;
                        Nonce12 : in Util.Encoders.AEAD.Nonce);

   --  Authenticate additional data (AAD) before reading the first data.
   --  The AAD must match the one used when encrypting the stream.
   procedure Update_AAD (Stream : in out Decoding_Stream;
                         Data   : in Ada.Streams.Stream_Element_Array);

end Util.Streams.AEAD;
