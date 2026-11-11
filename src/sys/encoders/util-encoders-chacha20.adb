-----------------------------------------------------------------------
--  util-encoders-chacha20 -- ChaCha20 stream cipher (RFC 8439)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Encoders.ChaCha20 is

   use type Interfaces.Unsigned_32;

   type ChaCha_State is array (0 .. 15) of Interfaces.Unsigned_32;

   procedure Quarter_Round (S : in out ChaCha_State; A, B, C, D : in Natural);

   function Rotl32 (V : Interfaces.Unsigned_32; N : in Natural) return Interfaces.Unsigned_32 is
     (if N = 0 then V
      else Interfaces.Shift_Left (V, N) or Interfaces.Shift_Right (V, 32 - N));

   procedure Quarter_Round (S : in out ChaCha_State; A, B, C, D : in Natural) is
   begin
      S (A) := S (A) + S (B); S (D) := Rotl32 (S (D) xor S (A), 16);
      S (C) := S (C) + S (D); S (B) := Rotl32 (S (B) xor S (C), 12);
      S (A) := S (A) + S (B); S (D) := Rotl32 (S (D) xor S (A), 8);
      S (C) := S (C) + S (D); S (B) := Rotl32 (S (B) xor S (C), 7);
   end Quarter_Round;

   --  ------------------------------
   --  The ChaCha20 block function: 64-byte keystream block for the given
   --  key, counter and 96-bit nonce.
   --  ------------------------------
   procedure Block (Key32   : in Key;
                    Counter : in Interfaces.Unsigned_32;
                    Nonce12 : in Nonce;
                    Block   : out Ada.Streams.Stream_Element_Array) is
      S : ChaCha_State
        := [0 => 16#61707865#, 1 => 16#3320646E#, 2 => 16#79622D32#, 3 => 16#6B206574#,
            12 => Counter, others => 0];
      W : ChaCha_State := S;
   begin
      for I in 0 .. 7 loop
         S (4 + I) := Load_LE32 (Key32.Secret, Key32.Secret'First + 4 * Stream_Element_Offset (I));
      end loop;
      for I in 0 .. 2 loop
         S (13 + I) := Load_LE32 (Nonce12, Nonce12'First + 4 * Stream_Element_Offset (I));
      end loop;
      W := S;
      for I in 1 .. 10 loop
         Quarter_Round (W, 0, 4, 8, 12);
         Quarter_Round (W, 1, 5, 9, 13);
         Quarter_Round (W, 2, 6, 10, 14);
         Quarter_Round (W, 3, 7, 11, 15);
         Quarter_Round (W, 0, 5, 10, 15);
         Quarter_Round (W, 1, 6, 11, 12);
         Quarter_Round (W, 2, 7, 8, 13);
         Quarter_Round (W, 3, 4, 9, 14);
      end loop;
      for I in 0 .. 15 loop
         Store_LE32 (W (I) + S (I), Block, Block'First + 4 * Stream_Element_Offset (I));
      end loop;
   end Block;

   --  ------------------------------
   --  XOR the data with the ChaCha20 keystream starting at First_Counter.
   --  ------------------------------
   procedure Crypt (Key32         : in Key;
                    Nonce12       : in Nonce;
                    First_Counter : in Interfaces.Unsigned_32;
                    Data          : in Ada.Streams.Stream_Element_Array;
                    Output        : out Ada.Streams.Stream_Element_Array) is
      Counter   : Interfaces.Unsigned_32 := First_Counter;
      Keystream : Ada.Streams.Stream_Element_Array (1 .. 64);
      Pos       : Stream_Element_Offset := Data'First;
      Out_Pos   : Stream_Element_Offset := Output'First;
      Remaining : Natural := Data'Length;
   begin
      while Remaining > 0 loop
         Block (Key32, Counter, Nonce12, Keystream);
         declare
            Take : constant Natural := Natural'Min (64, Remaining);
         begin
            for I in 0 .. Take - 1 loop
               Output (Out_Pos + Stream_Element_Offset (I))
                 := Data (Pos + Stream_Element_Offset (I))
                   xor Keystream (Stream_Element_Offset (1 + I));
            end loop;
            Pos := Pos + Stream_Element_Offset (Take);
            Out_Pos := Out_Pos + Stream_Element_Offset (Take);
            Remaining := Remaining - Take;
            Counter := Counter + 1;
         end;
      end loop;
   end Crypt;

   --  ------------------------------
   --  Set the 256-bit encryption key.
   --  ------------------------------
   procedure Set_Key (E    : in out Cipher;
                      Data : in Secret_Key) is
   begin
      E.Cipher_Key.Secret := Data.Secret;
      E.Key_Set := True;
      E.Keystream_Pos := Block_Size + 1;
   end Set_Key;

   --  ------------------------------
   --  Set the 96-bit nonce.
   --  ------------------------------
   procedure Set_Nonce (E       : in out Cipher;
                        Nonce12 : in Nonce) is
   begin
      E.Cipher_Nonce := Nonce12;
      E.Keystream_Pos := Block_Size + 1;
   end Set_Nonce;

   --  ------------------------------
   --  Set the initial 32-bit block counter.
   --  ------------------------------
   procedure Set_Counter (E       : in out Cipher;
                          Counter : in Interfaces.Unsigned_32) is
   begin
      E.Counter := Counter;
      E.Keystream_Pos := Block_Size + 1;
   end Set_Counter;

   --  ------------------------------
   --  Return true if the cipher has an encryption key configured.
   --  ------------------------------
   function Has_Key (E : in Cipher) return Boolean is
   begin
      return E.Key_Set;
   end Has_Key;

   --  ------------------------------
   --  Transform the data by XOR-ing it with the ChaCha20 keystream.
   --  The keystream block is generated when needed and kept in the cipher
   --  so that successive calls continue the keystream.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Encoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is
      Pos     : Stream_Element_Offset := Data'First;
      Out_Pos : Stream_Element_Offset := Into'First;
   begin
      while Pos <= Data'Last and then Out_Pos <= Into'Last loop
         if E.Keystream_Pos > Block_Size then
            Block (E.Cipher_Key, E.Counter, E.Cipher_Nonce, E.Keystream);
            E.Counter := E.Counter + 1;
            E.Keystream_Pos := 1;
         end if;
         Into (Out_Pos) := Data (Pos) xor E.Keystream (E.Keystream_Pos);
         Pos := Pos + 1;
         Out_Pos := Out_Pos + 1;
         E.Keystream_Pos := E.Keystream_Pos + 1;
      end loop;
      Last := Out_Pos - 1;
      Encoded := Pos - 1;
   end Transform;

   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is
      Pos     : Stream_Element_Offset := Data'First;
      Out_Pos : Stream_Element_Offset := Into'First;
   begin
      while Pos <= Data'Last and then Out_Pos <= Into'Last loop
         if E.Keystream_Pos > Block_Size then
            Block (E.Cipher_Key, E.Counter, E.Cipher_Nonce, E.Keystream);
            E.Counter := E.Counter + 1;
            E.Keystream_Pos := 1;
         end if;
         Into (Out_Pos) := Data (Pos) xor E.Keystream (E.Keystream_Pos);
         Pos := Pos + 1;
         Out_Pos := Out_Pos + 1;
         E.Keystream_Pos := E.Keystream_Pos + 1;
      end loop;
      Last := Out_Pos - 1;
      Encoded := Pos - 1;
   end Transform;

   overriding
   procedure Finalize (Object : in out Cipher) is
   begin
      Object.Cipher_Nonce := [others => 0];
      Object.Keystream := [others => 0];
   end Finalize;

end Util.Encoders.ChaCha20;
