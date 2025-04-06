-----------------------------------------------------------------------
--  encrypt_array -- Encrypt and convert to base64
--  Copyright (C) 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Util.Encoders.AES;
with Util.Encoders.KDF.PBKDF2_HMAC_SHA256;

procedure Encrypt_Array is

   use Ada.Streams;
   subtype Secret_Key is Util.Encoders.Secret_Key;
   package AES renames Util.Encoders.AES;

   function Encrypt (Key   : in Secret_Key;
                     Src   : in Stream_Element_Array) return Stream_Element_Array;
   procedure Create_Key (Password : in String;
                         Key      : in out Secret_Key);

   function Encrypt (Key   : in Secret_Key;
                     Src   : in Stream_Element_Array) return Stream_Element_Array is
      Size    : constant Stream_Element_Offset := AES.Align (Src'Length);
      Cipher  : Util.Encoders.AES.Encoder;
      Result  : Stream_Element_Array (1 .. Size);
      Encoded, Last : Stream_Element_Offset;
   begin
      --  Setup encryption key, mode and padding (IV could also be set here).
      Cipher.Set_Key (Key, AES.ECB);
      Cipher.Set_Padding (AES.PKCS7_PADDING);
      Cipher.Transform (Src, Result (Result'First .. Result'Last - 16), Last, Encoded);

      --  Because the destination is large enough, we have the following assertion.
      --  If the Result is not big enough, Transform must be called again in
      --  another buffer.
      pragma Assert (Encoded = Src'Last);

      Cipher.Finish (Result (Last + 1 .. Result'Last), Last);
      pragma Assert (Last = Result'Last);
      return Result;
   end Encrypt;

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
      Ada.Text_IO.Put_Line ("Usage: encrypt_array key content");
      Ada.Text_IO.Put_Line ("This command is similar to executing the OpenSSL command:");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("echo -n <content> | openssl aes-256-ecb -pbkdf2"
                            & " -S 66616B6573616C74 -iter 20000"
                            & " -a -e -k <key>");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("(see also decrypt_array example)");
      return;
   end if;
   Create_Key (Ada.Command_Line.Argument (1), Key);
   declare
      C    : constant String := Ada.Command_Line.Argument (2);
      B64  : constant Util.Encoders.Encoder := Util.Encoders.Create (Util.Encoders.BASE_64);
      Src  : Stream_Element_Array (1 .. C'Length);
      for Src'Address use C'Address;
   begin
      Ada.Text_IO.Put_Line (Util.Encoders.Encode_Binary (B64, Encrypt (Key, Src)));
   end;
end Encrypt_Array;
