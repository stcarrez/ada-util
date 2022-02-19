-----------------------------------------------------------------------
--  util-streams-aes -- AES encoding and decoding streams
--  Copyright (C) 2019, 2022 Stephane Carrez
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

with Util.Encoders.AES;
with Util.Streams.Buffered.Encoders;

--  == AES Encoding Streams ==
--  The `Util.Streams.AES` package define the `Encoding_Stream` and `Decoding_Stream` types to
--  encrypt and decrypt using the AES cipher.  Before using these streams, you must use
--  the `Set_Key` procedure to setup the encryption or decryption key and define the AES
--  encryption mode to be used.  The following encryption modes are supported:
--
--  * AES-ECB
--  * AES-CBC
--  * AES-PCBC
--  * AES-CFB
--  * AES-OFB
--  * AES-CTR
--
--  The encryption and decryption keys are represented by the `Util.Encoders.Secret_Key` limited
--  type.  The key cannot be copied, has its content protected and will erase the memory once
--  the instance is deleted.  The size of the encryption key defines the AES encryption level
--  to be used:
--
--  * Use 16 bytes, or `Util.Encoders.AES.AES_128_Length` for AES-128,
--  * Use 24 bytes, or `Util.Encoders.AES.AES_192_Length` for AES-192,
--  * Use 32 bytes, or `Util.Encoders.AES.AES_256_Length` for AES-256.
--
--  Other key sizes will raise a pre-condition or constraint error exception.
--  The recommended key size is 32 bytes to use AES-256.  The key could be declared as follows:
--
--    Key : Util.Encoders.Secret_Key
--              (Length => Util.Encoders.AES.AES_256_Length);
--
--  The encryption and decryption key are initialized by using the `Util.Encoders.Create`
--  operations or by using one of the key derivative functions provided by the
--  `Util.Encoders.KDF` package.  A simple string password is created by using:
--
--    Password_Key : constant Util.Encoders.Secret_Key
--              := Util.Encoders.Create ("mysecret");
--
--  Using a password key like this is not the good practice and it may be useful to generate
--  a stronger key by using one of the key derivative function.  We will use the
--  PBKDF2 HMAC-SHA256 with 20000 loops (see RFC 8018):
--
--    Util.Encoders.KDF.PBKDF2_HMAC_SHA256 (Password => Password_Key,
--                                          Salt     => Password_Key,
--                                          Counter  => 20000,
--                                          Result   => Key);
--
--  To write a text, encrypt the content and save the file, we can chain several stream objects
--  together.  Because they are chained, the last stream object in the chain must be declared
--  first and the first element of the chain will be declared last.  The following declaration
--  is used:
--
--      Out_Stream   : aliased Util.Streams.Files.File_Stream;
--      Cipher       : aliased Util.Streams.AES.Encoding_Stream;
--      Printer      : Util.Streams.Texts.Print_Stream;
--
--  The stream objects are chained together by using their `Initialize` procedure.
--  The `Out_Stream` is configured to write on the `encrypted.aes` file.
--  The `Cipher` is configured to write in the `Out_Stream` with a 32Kb buffer.
--  The `Printer` is configured to write in the `Cipher` with a 4Kb buffer.
--
--      Out_Stream.Initialize (Mode => Ada.Streams.Stream_IO.In_File,
--                             Name => "encrypted.aes");
--      Cipher.Initialize (Output => Out_Stream'Unchecked_Access,
--                         Size   => 32768);
--      Printer.Initialize (Output => Cipher'Unchecked_Access,
--                          Size   => 4096);
--
--  The last step before using the cipher is to configure the encryption key and modes:
--
--      Cipher.Set_Key (Secret => Key, Mode => Util.Encoders.AES.ECB);
--
--  It is now possible to write the text by using the `Printer` object:
--
--    Printer.Write ("Hello world!");
--
--
package Util.Streams.AES is

   package Encoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.AES.Encoder);

   package Decoding is
      new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.AES.Decoder);

   type Encoding_Stream is new Encoding.Encoder_Stream with null record;

   --  Set the encryption key and mode to be used.
   procedure Set_Key (Stream : in out Encoding_Stream;
                      Secret : in Util.Encoders.Secret_Key;
                      Mode   : in Util.Encoders.AES.AES_Mode := Util.Encoders.AES.CBC);

   --  Set the encryption initialization vector before starting the encryption.
   procedure Set_IV (Stream  : in out Encoding_Stream;
                     IV      : in Util.Encoders.AES.Word_Block_Type);

   type Decoding_Stream is new Decoding.Encoder_Stream with null record;

   --  Set the encryption key and mode to be used.
   procedure Set_Key (Stream : in out Decoding_Stream;
                      Secret : in Util.Encoders.Secret_Key;
                      Mode   : in Util.Encoders.AES.AES_Mode := Util.Encoders.AES.CBC);

   --  Set the encryption initialization vector before starting the encryption.
   procedure Set_IV (Stream  : in out Decoding_Stream;
                     IV      : in Util.Encoders.AES.Word_Block_Type);

end Util.Streams.AES;
