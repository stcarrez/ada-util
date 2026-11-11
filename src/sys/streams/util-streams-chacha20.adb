-----------------------------------------------------------------------
--  util-streams-chacha20 -- ChaCha20 encoding and decoding streams
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Streams.ChaCha20 is

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
                        Nonce12 : in Util.Encoders.ChaCha20.Nonce) is
   begin
      Stream.Transform.Set_Nonce (Nonce12);
   end Set_Nonce;

   --  -----------------------
   --  Set the initial block counter before starting the encryption.
   --  -----------------------
   procedure Set_Counter (Stream  : in out Encoding_Stream;
                          Counter : in Interfaces.Unsigned_32) is
   begin
      Stream.Transform.Set_Counter (Counter);
   end Set_Counter;

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
                        Nonce12 : in Util.Encoders.ChaCha20.Nonce) is
   begin
      Stream.Transform.Set_Nonce (Nonce12);
   end Set_Nonce;

   --  -----------------------
   --  Set the initial block counter before starting the decryption.
   --  -----------------------
   procedure Set_Counter (Stream  : in out Decoding_Stream;
                          Counter : in Interfaces.Unsigned_32) is
   begin
      Stream.Transform.Set_Counter (Counter);
   end Set_Counter;

end Util.Streams.ChaCha20;
