-----------------------------------------------------------------------
--  lzma_encrypt -- Compress and encrypt file using Util.Streams.AES
--  Copyright (C) 2019, 2021, 2022 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Util.Streams.Files;
with Util.Streams.AES;
with Util.Streams.Lzma;
with Util.Encoders.AES;
with Util.Encoders.KDF.PBKDF2_HMAC_SHA256;
procedure Lzma_Encrypt is

   use Util.Encoders.KDF;

   procedure Crypt_File (Source      : in String;
                         Destination : in String;
                         Password    : in String);

   procedure Crypt_File (Source      : in String;
                         Destination : in String;
                         Password    : in String) is
      In_Stream    : aliased Util.Streams.Files.File_Stream;
      Out_Stream   : aliased Util.Streams.Files.File_Stream;
      Cipher       : aliased Util.Streams.AES.Encoding_Stream;
      Compressor   : aliased Util.Streams.Lzma.Compress_Stream;
      Password_Key : constant Util.Encoders.Secret_Key := Util.Encoders.Create (Password);
      Salt         : constant Util.Encoders.Secret_Key := Util.Encoders.Create ("fake-salt");
      Key          : Util.Encoders.Secret_Key (Length => Util.Encoders.AES.AES_256_Length);
   begin
      --  Generate a derived key from the password.
      PBKDF2_HMAC_SHA256 (Password => Password_Key,
                          Salt     => Salt,
                          Counter  => 20000,
                          Result   => Key);

      --  Setup file -> input and compress -> cipher -> output file streams.
      In_Stream.Open (Ada.Streams.Stream_IO.In_File, Source);
      Out_Stream.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Destination);
      Cipher.Produces (Output => Out_Stream'Unchecked_Access, Size => 32768);
      Cipher.Set_Key (Secret => Key, Mode => Util.Encoders.AES.ECB);
      Compressor.Initialize (Output => Cipher'Unchecked_Access, Size => 4096);

      --  Copy input to output through the cipher.
      Util.Streams.Copy (From => In_Stream, Into => Compressor);
   end Crypt_File;

begin
   if Ada.Command_Line.Argument_Count /= 3 then
      Ada.Text_IO.Put_Line ("Usage: lzma_encrypt source password destination");
      return;
   end if;

   Crypt_File (Source      => Ada.Command_Line.Argument (1),
               Destination => Ada.Command_Line.Argument (3),
               Password    => Ada.Command_Line.Argument (2));
end Lzma_Encrypt;
