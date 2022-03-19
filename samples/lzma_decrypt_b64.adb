-----------------------------------------------------------------------
--  lzma_decrypt_b64 -- Base64 decode, decrypt and decompress file
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
with Util.Streams.Base64;
with Util.Encoders.AES;
with Util.Encoders.KDF.PBKDF2_HMAC_SHA256;
procedure Lzma_Decrypt_B64 is

   use Util.Encoders.KDF;

   procedure Decrypt_File (Source      : in String;
                           Destination : in String;
                           Password    : in String);

   procedure Decrypt_File (Source      : in String;
                           Destination : in String;
                           Password    : in String) is
      In_Stream    : aliased Util.Streams.Files.File_Stream;
      Out_Stream   : aliased Util.Streams.Files.File_Stream;
      Base64       : aliased Util.Streams.Base64.Decoding_Stream;
      Decompress   : aliased Util.Streams.Lzma.Decompress_Stream;
      Decipher     : aliased Util.Streams.AES.Decoding_Stream;
      Password_Key : constant Util.Encoders.Secret_Key := Util.Encoders.Create (Password);
      Salt         : constant Util.Encoders.Secret_Key := Util.Encoders.Create ("fake-salt");
      Key          : Util.Encoders.Secret_Key (Length => Util.Encoders.AES.AES_256_Length);
   begin
      --  Generate a derived key from the password.
      PBKDF2_HMAC_SHA256 (Password => Password_Key,
                          Salt     => Salt,
                          Counter  => 20000,
                          Result   => Key);

      --  Setup file -> input and cipher -> output file streams.
      In_Stream.Open (Ada.Streams.Stream_IO.In_File, Source);
      Out_Stream.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Destination);
      Base64.Consumes (Input => In_Stream'Unchecked_Access, Size => 32768);
      Decipher.Consumes (Input => Base64'Unchecked_Access, Size   => 32768);
      Decipher.Set_Key (Secret => Key, Mode => Util.Encoders.AES.ECB);
      Decompress.Initialize (Input => Decipher'Unchecked_Access, Size => 32768);

      --  Copy input to output through the cipher.
      Util.Streams.Copy (From => Decompress, Into => Out_Stream);
   end Decrypt_File;

begin
   if Ada.Command_Line.Argument_Count /= 3 then
      Ada.Text_IO.Put_Line ("Usage: lzma_decrypt_b64 source password destination");
      return;
   end if;

   Decrypt_File (Source      => Ada.Command_Line.Argument (1),
                 Destination => Ada.Command_Line.Argument (3),
                 Password    => Ada.Command_Line.Argument (2));
end Lzma_Decrypt_B64;
