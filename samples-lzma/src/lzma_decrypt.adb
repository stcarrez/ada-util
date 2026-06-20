-----------------------------------------------------------------------
--  lzma_decrypt -- Decrypt and decompress file using Util.Streams.AES
--  Copyright (C) 2019, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Util.Streams.Files;
with Util.Streams.AES;
with Util.Streams.Lzma;
with Util.Encoders.AES;
with Util.Encoders.KDF.PBKDF2_HMAC_SHA256;
procedure Lzma_Decrypt is

   use Util.Encoders.KDF;

   procedure Decrypt_File (Source      : in String;
                           Destination : in String;
                           Password    : in String);

   procedure Decrypt_File (Source      : in String;
                           Destination : in String;
                           Password    : in String) is
      In_Stream    : aliased Util.Streams.Files.File_Stream;
      Out_Stream   : aliased Util.Streams.Files.File_Stream;
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
      Decipher.Consumes (Input => In_Stream'Unchecked_Access, Size   => 32768);
      Decipher.Set_Key (Secret => Key, Mode => Util.Encoders.AES.ECB);
      Decompress.Initialize (Input => Decipher'Unchecked_Access, Size => 32768);

      --  Copy input to output through the cipher.
      Util.Streams.Copy (From => Decompress, Into => Out_Stream);
   end Decrypt_File;

begin
   if Ada.Command_Line.Argument_Count /= 3 then
      Ada.Text_IO.Put_Line ("Usage: lzma_decrypt source password destination");
      return;
   end if;

   Decrypt_File (Source      => Ada.Command_Line.Argument (1),
                 Destination => Ada.Command_Line.Argument (3),
                 Password    => Ada.Command_Line.Argument (2));
end Lzma_Decrypt;
