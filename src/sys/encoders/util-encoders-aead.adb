-----------------------------------------------------------------------
--  util-encoders-aead -- ChaCha20-Poly1305 AEAD (RFC 8439)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Encoders.AEAD is

   use Interfaces;

   Zeros : constant Stream_Element_Array (1 .. 16) := [others => 0];

   --  ------------------------------
   --  Write a 64-bit value as 8 bytes little-endian.
   --  ------------------------------
   procedure Put_LE64 (Into  : out Stream_Element_Array;
                       Value : in  Unsigned_64) is
   begin
      for I in 0 .. 7 loop
         Into (Into'First + Stream_Element_Offset (I))
           := Stream_Element (Shift_Right (Value, 8 * I) and 16#FF#);
      end loop;
   end Put_LE64;

   --  ------------------------------
   --  Copy the cipher key into a Secret_Key view.
   --  ------------------------------
   procedure Get_Key (E : in Cipher'Class; K : out Key) is
   begin
      Create (E.Cipher_Key, K);
   end Get_Key;

   --  ------------------------------
   --  Derive the one-time Poly1305 key from the counter-0 ChaCha20 block and
   --  reset the message state.  The keystream encryption starts at counter 1.
   --  ------------------------------
   procedure Start (E : in out Cipher'Class) is
      Block0 : Block_Type;
      K      : Key;
   begin
      if not E.Started then
         Get_Key (E, K);
         Util.Encoders.ChaCha20.Block (K, 0, E.Cipher_Nonce, Block0);
         Util.Encoders.Poly1305.Set_Key (E.Mac, Block0 (1 .. 32));
         E.Counter       := 1;
         E.Keystream_Pos := Block_Type'Last + 1;
         E.Started       := True;
         E.AAD_Len       := 0;
         E.Data_Len      := 0;
         E.AAD_Padded    := False;
         E.Hold_Len      := 0;
      end if;
   end Start;

   --  ------------------------------
   --  Insert the pad16 of the AAD before the first ciphertext byte is
   --  authenticated (RFC 8439 section 2.8).
   --  ------------------------------
   procedure Begin_Data (E : in out Cipher'Class) is
      Pad : constant Natural := (16 - Natural (E.AAD_Len mod 16)) mod 16;
   begin
      if not E.AAD_Padded then
         if Pad > 0 then
            Util.Encoders.Poly1305.Update (E.Mac, Zeros (1 .. Stream_Element_Offset (Pad)));
         end if;
         E.AAD_Padded := True;
      end if;
   end Begin_Data;

   --  ------------------------------
   --  Generate the next keystream block when the current one is exhausted.
   --  ------------------------------
   procedure Next_Block (E : in out Cipher'Class) is
      K : Key;
   begin
      if E.Keystream_Pos > Block_Type'Last then
         Get_Key (E, K);
         Util.Encoders.ChaCha20.Block (K, E.Counter, E.Cipher_Nonce, E.Keystream);
         E.Counter       := E.Counter + 1;
         E.Keystream_Pos := 1;
      end if;
   end Next_Block;

   --  ------------------------------
   --  Set the 256-bit AEAD key.
   --  ------------------------------
   procedure Set_Key (E    : in out Cipher;
                      Data : in     Secret_Key) is
   begin
      E.Cipher_Key := Data.Secret;
      E.Key_Set  := True;
      E.Started  := False;
      E.Hold_Len := 0;
   end Set_Key;

   --  ------------------------------
   --  Set the 96-bit nonce.
   --  ------------------------------
   procedure Set_Nonce (E       : in out Cipher;
                        Nonce12 : in     Nonce) is
   begin
      E.Cipher_Nonce := Nonce12;
      E.Nonce_Set    := True;
      E.Started      := False;
      E.Hold_Len     := 0;
   end Set_Nonce;

   function Has_Key (E : in Cipher) return Boolean is
   begin
      return E.Key_Set;
   end Has_Key;

   function Has_Nonce (E : in Cipher) return Boolean is
   begin
      return E.Nonce_Set;
   end Has_Nonce;

   --  ------------------------------
   --  Authenticate additional data (before any Transform call).
   --  ------------------------------
   procedure Update_AAD (E    : in out Cipher;
                         Data : in     Stream_Element_Array) is
   begin
      Start (E);
      if E.Data_Len > 0 then
         raise Encoding_Error with "AEAD: AAD must be set before the data";
      end if;
      Util.Encoders.Poly1305.Update (E.Mac, Data);
      E.AAD_Len := E.AAD_Len + Unsigned_64 (Data'Length);
   end Update_AAD;

   --  ------------------------------
   --  Encrypt the data: XOR with the ChaCha20 keystream and authenticate
   --  the produced ciphertext with Poly1305.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Encoder;
                        Data    : in     Stream_Element_Array;
                        Into    : out    Stream_Element_Array;
                        Last    : out    Stream_Element_Offset;
                        Encoded : out    Stream_Element_Offset) is
      Pos     : Stream_Element_Offset := Data'First;
      Out_Pos : Stream_Element_Offset := Into'First;
   begin
      Start (E);
      Begin_Data (E);
      while Pos <= Data'Last and then Out_Pos <= Into'Last loop
         Next_Block (E);
         Into (Out_Pos) := Data (Pos) xor E.Keystream (E.Keystream_Pos);
         E.Keystream_Pos := E.Keystream_Pos + 1;
         Pos     := Pos + 1;
         Out_Pos := Out_Pos + 1;
      end loop;

      --  Authenticate the ciphertext produced by this call.
      if Out_Pos > Into'First then
         Util.Encoders.Poly1305.Update (E.Mac, Into (Into'First .. Out_Pos - 1));
         E.Data_Len := E.Data_Len + Unsigned_64 (Out_Pos - Into'First);
      end if;
      Last    := Out_Pos - 1;
      Encoded := Pos - 1;
   end Transform;

   --  ------------------------------
   --  Finish the encryption and write the 16-byte tag.
   --  ------------------------------
   overriding
   procedure Finish (E    : in out Encoder;
                     Into : in out Stream_Element_Array;
                     Last : in out Stream_Element_Offset) is
      Pad       : constant Natural := (16 - Natural (E.Data_Len mod 16)) mod 16;
      Len_Block : Stream_Element_Array (1 .. 16) := [others => 0];
      Computed  : Tag;
   begin
      Start (E);
      Begin_Data (E);
      if Pad > 0 then
         Util.Encoders.Poly1305.Update (E.Mac, Zeros (1 .. Stream_Element_Offset (Pad)));
      end if;
      Put_LE64 (Len_Block (1 .. 8), E.AAD_Len);
      Put_LE64 (Len_Block (9 .. 16), E.Data_Len);
      Util.Encoders.Poly1305.Update (E.Mac, Len_Block);
      Util.Encoders.Poly1305.Finish (E.Mac, Computed);
      Into (Into'First .. Into'First + Tag_Size - 1) := Computed;
      Last := Into'First + Tag_Size - 1;
   end Finish;

   --  ------------------------------
   --  Decrypt the data: hold back the last 16 bytes of the stream (the tag),
   --  authenticate and decrypt the other bytes with the ChaCha20 keystream.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in     Stream_Element_Array;
                        Into    : out    Stream_Element_Array;
                        Last    : out    Stream_Element_Offset;
                        Encoded : out    Stream_Element_Offset) is
      Pos     : Stream_Element_Offset := Data'First;
      Out_Pos : Stream_Element_Offset := Into'First;
   begin
      Start (E);
      Begin_Data (E);
      while Pos <= Data'Last
        and then (E.Hold_Len < Tag_Size or else Out_Pos <= Into'Last)
      loop
         if E.Hold_Len = Tag_Size then
            --  The oldest held byte is ciphertext: authenticate and decrypt it.
            Next_Block (E);
            Util.Encoders.Poly1305.Update (E.Mac, E.Hold (1 .. 1));
            E.Data_Len := E.Data_Len + 1;
            Into (Out_Pos) := E.Hold (1) xor E.Keystream (E.Keystream_Pos);
            E.Keystream_Pos := E.Keystream_Pos + 1;
            Out_Pos := Out_Pos + 1;
            E.Hold (1 .. Tag_Size - 1) := E.Hold (2 .. Tag_Size);
            E.Hold_Len := Tag_Size - 1;
         end if;
         E.Hold_Len := E.Hold_Len + 1;
         E.Hold (Stream_Element_Offset (E.Hold_Len)) := Data (Pos);
         Pos := Pos + 1;
      end loop;
      Last    := Out_Pos - 1;
      Encoded := Pos - 1;
   end Transform;

   --  ------------------------------
   --  Verify the tag held back from the input stream.
   --  ------------------------------
   overriding
   procedure Finish (E    : in out Decoder;
                     Into : in out Stream_Element_Array;
                     Last : in out Stream_Element_Offset) is
      pragma Unreferenced (Into, Last);
      Pad       : constant Natural := (16 - Natural (E.Data_Len mod 16)) mod 16;
      Len_Block : Stream_Element_Array (1 .. 16) := [others => 0];
      Computed  : Tag;
      Diff      : Stream_Element := 0;
   begin
      Start (E);
      Begin_Data (E);
      if E.Hold_Len /= Tag_Size then
         raise Encoding_Error with "AEAD: missing authentication tag";
      end if;
      if Pad > 0 then
         Util.Encoders.Poly1305.Update (E.Mac, Zeros (1 .. Stream_Element_Offset (Pad)));
      end if;
      Put_LE64 (Len_Block (1 .. 8), E.AAD_Len);
      Put_LE64 (Len_Block (9 .. 16), E.Data_Len);
      Util.Encoders.Poly1305.Update (E.Mac, Len_Block);
      Util.Encoders.Poly1305.Finish (E.Mac, Computed);
      for I in Computed'Range loop
         Diff := Diff or (Computed (I) xor E.Hold (I));
      end loop;
      if Diff /= 0 then
         raise Encoding_Error with "AEAD: invalid authentication tag";
      end if;
   end Finish;

   overriding
   procedure Finalize (Object : in out Cipher) is
   begin
      Object.Cipher_Key := [others => 0];
      Object.Cipher_Nonce      := [others => 0];
      Object.Keystream         := [others => 0];
      Object.Hold              := [others => 0];
   end Finalize;

end Util.Encoders.AEAD;
