-----------------------------------------------------------------------
--  util-streams-aead -- AEAD (ChaCha20-Poly1305) encoding and decoding streams
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Streams.AEAD is

   --  -----------------------
   --  Set the encryption key to be used.
   --  -----------------------
   procedure Set_Key (Stream : in out Encoding_Stream;
                      Secret : in Util.Encoders.Secret_Key) is
   begin
      Stream.Transform.Set_Key (Secret);
   end Set_Key;

   --  -----------------------
   --  Set the nonce before starting the encryption.
   --  -----------------------
   procedure Set_Nonce (Stream  : in out Encoding_Stream;
                        Nonce12 : in Util.Encoders.AEAD.Nonce) is
   begin
      Stream.Transform.Set_Nonce (Nonce12);
   end Set_Nonce;

   --  -----------------------
   --  Authenticate additional data before starting the encryption.
   --  -----------------------
   procedure Update_AAD (Stream : in out Encoding_Stream;
                         Data   : in Ada.Streams.Stream_Element_Array) is
   begin
      Stream.Transform.Update_AAD (Data);
   end Update_AAD;

   --  -----------------------
   --  Set the decryption key to be used.
   --  -----------------------
   procedure Set_Key (Stream : in out Decoding_Stream;
                      Secret : in Util.Encoders.Secret_Key) is
   begin
      Stream.Transform.Set_Key (Secret);
   end Set_Key;

   --  -----------------------
   --  Set the nonce before starting the decryption.
   --  -----------------------
   procedure Set_Nonce (Stream  : in out Decoding_Stream;
                        Nonce12 : in Util.Encoders.AEAD.Nonce) is
   begin
      Stream.Transform.Set_Nonce (Nonce12);
   end Set_Nonce;

   --  -----------------------
   --  Authenticate additional data before starting the decryption.
   --  -----------------------
   procedure Update_AAD (Stream : in out Decoding_Stream;
                         Data   : in Ada.Streams.Stream_Element_Array) is
   begin
      Stream.Transform.Update_AAD (Data);
   end Update_AAD;

end Util.Streams.AEAD;
