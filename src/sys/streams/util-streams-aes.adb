-----------------------------------------------------------------------
--  util-streams-aes -- AES encoding and decoding streams
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Streams.AES is

   --  -----------------------
   --  Set the encryption key and mode to be used.
   --  -----------------------
   procedure Set_Key (Stream : in out Encoding_Stream;
                      Secret : in Util.Encoders.Secret_Key;
                      Mode   : in Util.Encoders.AES.AES_Mode := Util.Encoders.AES.CBC) is
   begin
      Stream.Transform.Set_Key (Secret, Mode);
   end Set_Key;

   --  -----------------------
   --  Set the encryption initialization vector before starting the encryption.
   --  -----------------------
   procedure Set_IV (Stream  : in out Encoding_Stream;
                     IV      : in Util.Encoders.AES.Word_Block_Type) is
   begin
      Stream.Transform.Set_IV (IV);
   end Set_IV;

   --  -----------------------
   --  Set the encryption key and mode to be used.
   --  -----------------------
   procedure Set_Key (Stream : in out Decoding_Stream;
                      Secret : in Util.Encoders.Secret_Key;
                      Mode   : in Util.Encoders.AES.AES_Mode := Util.Encoders.AES.CBC) is
   begin
      Stream.Transform.Set_Key (Secret, Mode);
   end Set_Key;

   --  -----------------------
   --  Set the encryption initialization vector before starting the encryption.
   --  -----------------------
   procedure Set_IV (Stream  : in out Decoding_Stream;
                     IV      : in Util.Encoders.AES.Word_Block_Type) is
   begin
      Stream.Transform.Set_IV (IV);
   end Set_IV;

end Util.Streams.AES;
