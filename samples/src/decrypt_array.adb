-----------------------------------------------------------------------
--  decrypt_array -- Decrypt after converting from base64
--  Copyright (C) 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Util.Encoders.AES;
with Util.Encoders.KDF.PBKDF2_HMAC_SHA256;

procedure Decrypt_Array is

   use Ada.Streams;
   subtype Secret_Key is Util.Encoders.Secret_Key;
   package AES renames Util.Encoders.AES;

   function Decrypt (Key   : in Secret_Key;
                     Src   : in Stream_Element_Array) return Stream_Element_Array;
   procedure Create_Key (Password : in String;
                         Key      : in out Secret_Key);

   function Decrypt (Key   : in Secret_Key;
                     Src   : in Stream_Element_Array) return Stream_Element_Array is
      Size     : constant Stream_Element_Offset := Src'Length;
      Decipher : Util.Encoders.AES.Decoder;
      Result   : Stream_Element_Array (1 .. Size);
      Encoded, Last : Stream_Element_Offset;
   begin
      --  Setup encryption key, mode and padding (IV could also be set here).
      Decipher.Set_Key (Key, AES.ECB);
      Decipher.Set_Padding (AES.PKCS7_PADDING);
      Decipher.Transform (Src, Result, Last, Encoded);

      --  Because the destination is large enough, we have the following assertion.
      --  If the Result is not big enough, Transform must be called again in
      --  another buffer.
      pragma Assert (Encoded = Src'Last);

      Decipher.Finish (Result (Last + 1 .. Result'Last), Last);
      return Result (Result'First .. Last);
   end Decrypt;

   --  Generate a derived key from the password.
   procedure Create_Key (Password : in String;
                         Key      : in out Secret_Key) is
      use Util.Encoders.KDF;
      Pkey    : constant Secret_Key := Util.Encoders.Create (Password);
      Salt    : constant Secret_Key := Util.Encoders.Create ("fakesalt");
   begin
      PBKDF2_HMAC_SHA256 (Password => Pkey,
                          Salt     => Salt,
                          Counter  => 20000,
                          Result   => Key);
   end Create_Key;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;
   Key    : Util.Encoders.Secret_Key (Length => AES.AES_256_Length);
begin
   if Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: decrypt_array key base64-content");
      Ada.Text_IO.Put_Line ("This command is similar to executing the OpenSSL command:");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("echo <content> | openssl aes-256-ecb -pbkdf2"
                            & " -S 66616B6573616C74 -iter 20000"
                            & " -a -d -k <key>");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("(see also encrypt_array example)");
      return;
   end if;
   Create_Key (Ada.Command_Line.Argument (1), Key);
   declare
      C    : constant String := Ada.Command_Line.Argument (2);
      B64  : constant Util.Encoders.Decoder := Util.Encoders.Create (Util.Encoders.BASE_64);
      Src  : constant Stream_Element_Array := Util.Encoders.Decode_Binary (B64, C);
      Data : constant Stream_Element_Array := Decrypt (Key, Src);
      Res  : String (1 .. Data'Length);
      for Res'Address use Data'Address;
   begin
      Ada.Text_IO.Put_Line (Res);
   end;
end Decrypt_Array;
