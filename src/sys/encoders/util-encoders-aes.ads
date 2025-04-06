-----------------------------------------------------------------------
--  util-encoders-aes -- AES encryption and decryption
--  Copyright (C) 2017, 2019, 2020, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Interfaces;
private with Ada.Finalization;

--  == AES Encryption and decryption ==
--  The `Util.Encoders.AES` package implements the basis for AES encryption
--  and decryption used by the `Util.Streams.AES` package for AES streams.
--  Several encryption modes are available and described by the `AES_Mode`
--  type and the padding is controlled by the `AES_Padding` type.
--  The encryption is provided by methods on the `Encoder` type.  The `Set_Key`
--  procedure configures the encryption key and AES encryption mode.  The key
--  must be 16, 24 or 32 bytes as specified by the pre-condition.  Then, the
--  `Transform` procedure is called as needed to encrypt the content in a
--  destination buffer.  It returns the position of the last encoded byte as
--  well as the position of the last encrypted byte.  It can be called several
--  times if the data to encrypt does not fit in the destination buffer.
--  At the end, it is important to call the `Finish` procedure so that it
--  handles correctly the last AES block which may contain the last encrypted
--  bytes as well as optional padding.  An encryption function is shown below:
--
--    function Encrypt (Key   : in Secret_Key;
--                      Src   : in Stream_Element_Array) return Stream_Element_Array is
--       Size    : constant Stream_Element_Offset := AES.Align (Src'Length);
--       Cipher  : Util.Encoders.AES.Encoder;
--       Encoded, Last : Stream_Element_Offset;
--       Result  : Stream_Element_Array (1 .. Size);
--    begin
--       Cipher.Set_Key (Key, AES.ECB);
--       Cipher.Transform (Src, Result (Result'First .. Result'Last - 16),
--                         Last, Encoded);
--       Cipher.Finish (Result (Last + 1 .. Result'Last), Last);
--       return Result;
--    end Encrypt;
--
--  With AES, the encryption result is always a multiple of AES block size (16 bytes).
--  The `Align` function allows to easily compute the expected encrypted size.
--
--  Decrypting a content follows the same principles but with the `Decoder` type.
--  The source array must be a multiple of AES block and the last AES block may
--  contain some padding.  The `Finish` procedure will then indicate the last
--  valid byte in the decrypted buffer.
--
--    function Decrypt (Key   : in Secret_Key;
--                      Src   : in Stream_Element_Array) return Stream_Element_Array is
--       Size     : constant Stream_Element_Offset := Src'Length;
--       Decipher : Util.Encoders.AES.Decoder;
--       Result   : Stream_Element_Array (1 .. Size);
--       Encoded, Last : Stream_Element_Offset;
--    begin
--       Decipher.Set_Key (Key, AES.ECB);
--       Decipher.Set_Padding (AES.PKCS7_PADDING);
--       Decipher.Transform (Src, Result, Last, Encoded);
--       Decipher.Finish (Result (Last + 1 .. Result'Last), Last);
--       return Result (Result'First .. Last);
--    end Decrypt;
--
package Util.Encoders.AES is

   type AES_Mode is (ECB, CBC, PCBC, CFB, OFB, CTR);

   type AES_Padding is (NO_PADDING, ZERO_PADDING, PKCS7_PADDING);

   type Key_Type is private;

   --  ------------------------------
   --  ------------------------------
   subtype Block_Type is Ada.Streams.Stream_Element_Array (1 .. 16);

   AES_128_Length : constant := 16;
   AES_192_Length : constant := 24;
   AES_256_Length : constant := 32;

   subtype AES_128_Key is Ada.Streams.Stream_Element_Array (1 .. 16);
   subtype AES_192_Key is Ada.Streams.Stream_Element_Array (1 .. 24);
   subtype AES_256_Key is Ada.Streams.Stream_Element_Array (1 .. 32);

   --  Align the size on an AES block size.
   function Align (Size : in Ada.Streams.Stream_Element_Offset)
                   return Ada.Streams.Stream_Element_Offset is
      (Block_Type'Length * ((Size + Block_Type'Length - 1) / Block_Type'Length));

   type Word_Block_Type is array (1 .. 4) of Interfaces.Unsigned_32;

   procedure Set_Encrypt_Key (Key  : out Key_Type;
                              Data : in Secret_Key)
     with Pre => Data.Length in 16 | 24 | 32;

   procedure Set_Decrypt_Key (Key  : out Key_Type;
                              Data : in Secret_Key)
     with Pre => Data.Length in 16 | 24 | 32;

   procedure Encrypt (Data   : in out Word_Block_Type;
                      Key    : in Key_Type);
   procedure Encrypt (Input  : in Block_Type;
                      Output : out Block_Type;
                      Key    : in Key_Type);
   procedure Encrypt (Input  : in Word_Block_Type;
                      Output : out Word_Block_Type;
                      Key    : in Key_Type);

   --  Encrypt using AES-ECB (not recommended to use, prefer use of Encoder).
   procedure Encrypt (Input  : in Ada.Streams.Stream_Element_Array;
                      Output : out Ada.Streams.Stream_Element_Array;
                      Last   : out Ada.Streams.Stream_Element_Offset;
                      Key    : in Key_Type);

   procedure Decrypt (Input  : in Block_Type;
                      Output : out Block_Type;
                      Key    : in Key_Type);

   procedure Decrypt (Input  : in Word_Block_Type;
                      Output : out Word_Block_Type;
                      Key    : in Key_Type);

   type Cipher is tagged limited private;

   --  Set the encryption initialization vector before starting the encryption.
   procedure Set_IV (E  : in out Cipher;
                     IV : in Word_Block_Type);
   procedure Set_IV (E   : in out Cipher;
                     Key : in Secret_Key)
     with Pre => Key.Length = 16;
   procedure Set_IV (E   : in out Cipher;
                     Key : in Secret_Key;
                     IV  : in Word_Block_Type);

   --  Set the padding.
   procedure Set_Padding (E       : in out Cipher;
                          Padding : in AES_Padding);

   --  Get the padding used.
   function Padding (E : in Cipher) return AES_Padding;

   --  Return true if the cipher has a encryption/decryption key configured.
   function Has_Key (E : in Cipher) return Boolean;

   --  ------------------------------
   --  AES encoder
   --  ------------------------------
   --  This <b>Encoder</b> translates the (binary) input stream into
   --  an SHA1 hexadecimal stream.  The encoding alphabet is: 0123456789ABCDEF.
   type Encoder is new Cipher and Util.Encoders.Transformer with private;

   --  Set the encryption key to use.
   procedure Set_Key (E    : in out Encoder;
                      Data : in Secret_Key;
                      Mode : in AES_Mode := CBC)
     with Pre => Data.Length in 16 | 24 | 32;

   --  Encodes the binary input stream represented by <b>Data</b> into
   --  an SHA-1 hash output stream <b>Into</b>.
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

   --  Finish encoding the input array.
   overriding
   procedure Finish (E    : in out Encoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset) with
     Pre  => E.Has_Key and then Into'Length >= Block_Type'Length,
     Post => Last = Into'First - 1 or else Last = Into'First + Block_Type'Length - 1;

   --  Encrypt the secret using the encoder and return the encrypted value in the buffer.
   --  The target buffer must be a multiple of 16-bytes block.
   procedure Encrypt_Secret (E      : in out Encoder;
                             Secret : in Secret_Key;
                             Into   : out Ada.Streams.Stream_Element_Array) with
     Pre => Into'Length mod 16 = 0 and then
     (case E.Padding is
        when NO_PADDING => Secret.Length = Into'Length,
          when PKCS7_PADDING | ZERO_PADDING => 16 * (1 + (Secret.Length / 16)) = Into'Length);

   --  ------------------------------
   --  AES encoder
   --  ------------------------------
   --  This <b>Encoder</b> translates the (binary) input stream into
   --  an SHA1 hexadecimal stream.  The encoding alphabet is: 0123456789ABCDEF.
   type Decoder is new Cipher and Util.Encoders.Transformer with private;

   --  Set the decryption key to use.
   procedure Set_Key (E    : in out Decoder;
                      Data : in Secret_Key;
                      Mode : in AES_Mode := CBC)
     with Pre => Data.Length in 16 | 24 | 32;

   --  Encodes the binary input stream represented by <b>Data</b> into
   --  an SHA-1 hash output stream <b>Into</b>.
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
   procedure Transform (E       : in out Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) with
     Pre => E.Has_Key;

   --  Finish encoding the input array.
   overriding
   procedure Finish (E    : in out Decoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset);

   --  Decrypt the content using the decoder and build the secret key.
   procedure Decrypt_Secret (E      : in out Decoder;
                             Data   : in Ada.Streams.Stream_Element_Array;
                             Secret : in out Secret_Key) with
     Pre => Data'Length mod 16 = 0 and then
     (case E.Padding is
        when NO_PADDING => Secret.Length = Data'Length,
          when PKCS7_PADDING | ZERO_PADDING => 16 * (1 + (Secret.Length / 16)) = Data'Length);

private

   use Interfaces;

   subtype Count_Type is Ada.Streams.Stream_Element_Offset range 0 .. 16;

   type Block_Key is array (0 .. 59) of Unsigned_32;

   type Key_Type is record
      Key    : Block_Key := (others => 0);
      Rounds : Natural := 0;
   end record;

   type Cipher is limited new Ada.Finalization.Limited_Controlled with record
      IV         : Word_Block_Type := (others => 0);
      Key        : Key_Type;
      Mode       : AES_Mode := CBC;
      Padding    : AES_Padding := PKCS7_PADDING;
      Data_Count : Count_Type := 0;
      Data       : Block_Type;
      Data2      : Block_Type;
   end record;

   overriding
   procedure Finalize (Object : in out Cipher);

   type Encoder is new Cipher and Util.Encoders.Transformer with null record;

   type Decoder is new Cipher and Util.Encoders.Transformer with null record;

end Util.Encoders.AES;
