-----------------------------------------------------------------------
--  util-streams-chacha20 -- ChaCha20 encoding and decoding streams
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Interfaces;
with Util.Encoders;
with Util.Encoders.ChaCha20;
with Util.Streams.Buffered.Encoders;

--  == ChaCha20 Encoding Streams ==
--  The `Util.Streams.ChaCha20` package defines the `Encoding_Stream` and
--  `Decoding_Stream` types to encrypt and decrypt a stream by using the
--  ChaCha20 cipher (RFC 8439) provided by the `Util.Encoders.ChaCha20` package.
--  Before using these streams, you must use the `Set_Key` procedure to setup
--  the 256-bit encryption key and the `Set_Nonce` procedure to define the
--  96-bit nonce.  The nonce must be unique for each message encrypted with
--  the same key.  The initial 32-bit block counter can be changed with the
--  `Set_Counter` procedure (it is 0 by default).
--
--  The encryption and decryption keys are represented by the
--  `Util.Encoders.Secret_Key` limited type.  The key must be 32 bytes
--  (`Util.Encoders.ChaCha20.Key_Size`).
--
--  To write a text, encrypt the content and save the file, we can chain several
--  stream objects together.  Because they are chained, the last stream object in
--  the chain must be declared first and the first element of the chain will be
--  declared last.  The following declaration is used:
--
--      Out_Stream   : aliased Util.Streams.Files.File_Stream;
--      Cipher       : aliased Util.Streams.ChaCha20.Encoding_Stream;
--      Printer      : Util.Streams.Texts.Print_Stream;
--
--      Out_Stream.Initialize (Mode => Ada.Streams.Stream_IO.Out_File,
--                             Name => "encrypted.chacha20");
--      Cipher.Initialize (Output => Out_Stream'Unchecked_Access,
--                         Size   => 32768);
--      Printer.Initialize (Output => Cipher'Unchecked_Access,
--                          Size   => 4096);
--      Cipher.Set_Key (Key);
--      Cipher.Set_Nonce (Nonce12);
--
--  It is now possible to write the text by using the `Printer` object:
--
--    Printer.Write ("Hello world!");
--
package Util.Streams.ChaCha20 is

   package Encoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.ChaCha20.Encoder);

   package Decoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.ChaCha20.Decoder);

   type Encoding_Stream is new Encoding.Encoder_Stream with null record;

   --  Set the 256-bit encryption key to be used.
   procedure Set_Key (Stream : in out Encoding_Stream;
                      Secret : in Util.Encoders.Secret_Key);

   --  Set the 96-bit nonce before starting the encryption.
   procedure Set_Nonce (Stream  : in out Encoding_Stream;
                        Nonce12 : in Util.Encoders.ChaCha20.Nonce);

   --  Set the initial 32-bit block counter (0 by default).
   procedure Set_Counter (Stream  : in out Encoding_Stream;
                          Counter : in Interfaces.Unsigned_32);

   type Decoding_Stream is new Decoding.Encoder_Stream with null record;

   --  Set the 256-bit decryption key to be used.
   procedure Set_Key (Stream : in out Decoding_Stream;
                      Secret : in Util.Encoders.Secret_Key);

   --  Set the 96-bit nonce before starting the decryption.
   procedure Set_Nonce (Stream  : in out Decoding_Stream;
                        Nonce12 : in Util.Encoders.ChaCha20.Nonce);

   --  Set the initial 32-bit block counter (0 by default).
   procedure Set_Counter (Stream  : in out Decoding_Stream;
                          Counter : in Interfaces.Unsigned_32);

end Util.Streams.ChaCha20;
