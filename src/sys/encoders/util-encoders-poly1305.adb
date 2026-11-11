-----------------------------------------------------------------------
--  util-encoders-poly1305 -- Poly1305 one-time authenticator (RFC 8439)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Encoders.Poly1305 is

   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   --  Arithmetic modulo 2^130 - 5 with 5 limbs of 26 bits (poly1305-donna).
   procedure Poly1305_Mul (H : in out Poly5;
                           R0, R1, R2, R3, R4 : in Interfaces.Unsigned_32;
                           S1, S2, S3, S4     : in Interfaces.Unsigned_32) is
      W0 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (R0);
      W1 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (R1);
      W2 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (R2);
      W3 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (R3);
      W4 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (R4);
      V1 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (S1);
      V2 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (S2);
      V3 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (S3);
      V4 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (S4);
      H0 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (H (0));
      H1 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (H (1));
      H2 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (H (2));
      H3 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (H (3));
      H4 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (H (4));
      D0 : constant Interfaces.Unsigned_64 := H0 * W0 + H1 * V4 + H2 * V3 + H3 * V2 + H4 * V1;
      D1 : constant Interfaces.Unsigned_64 := H0 * W1 + H1 * W0 + H2 * V4 + H3 * V3 + H4 * V2;
      D2 : constant Interfaces.Unsigned_64 := H0 * W2 + H1 * W1 + H2 * W0 + H3 * V4 + H4 * V3;
      D3 : constant Interfaces.Unsigned_64 := H0 * W3 + H1 * W2 + H2 * W1 + H3 * W0 + H4 * V4;
      D4 : constant Interfaces.Unsigned_64 := H0 * W4 + H1 * W3 + H2 * W2 + H3 * W1 + H4 * W0;
      C : Interfaces.Unsigned_64;
      V : Interfaces.Unsigned_64;
   begin
      V := D0; H (0) := Interfaces.Unsigned_32 (V and 16#3FFFFFF#);
      C := Interfaces.Shift_Right (V, 26);
      V := D1 + C; H (1) := Interfaces.Unsigned_32 (V and 16#3FFFFFF#);
      C := Interfaces.Shift_Right (V, 26);
      V := D2 + C; H (2) := Interfaces.Unsigned_32 (V and 16#3FFFFFF#);
      C := Interfaces.Shift_Right (V, 26);
      V := D3 + C; H (3) := Interfaces.Unsigned_32 (V and 16#3FFFFFF#);
      C := Interfaces.Shift_Right (V, 26);
      V := D4 + C; H (4) := Interfaces.Unsigned_32 (V and 16#3FFFFFF#);
      C := Interfaces.Shift_Right (V, 26);
      H (0) := Interfaces.Unsigned_32 (Interfaces.Unsigned_64 (H (0)) + C * 5);
      C := Interfaces.Shift_Right (Interfaces.Unsigned_64 (H (0)), 26);
      H (0) := H (0) and 16#3FFFFFF#;
      H (1) := Interfaces.Unsigned_32 (Interfaces.Unsigned_64 (H (1)) + C);
   end Poly1305_Mul;

   --  ------------------------------
   --  Process one 16-byte block: add it to H (with the hibit for a full
   --  block, or the 2^(8*Take) pad bit already set in the buffer for a
   --  partial block) and multiply by r.
   --  ------------------------------
   procedure Process_Block (Ctx  : in out Context;
                            Buf  : in Block_Type;
                            Take : in Natural) is
      T0, T1, T2, T3 : Interfaces.Unsigned_32;
   begin
      T0 := Load_LE32 (Buf, Buf'First);
      T1 := Load_LE32 (Buf, Buf'First + 4);
      T2 := Load_LE32 (Buf, Buf'First + 8);
      T3 := Load_LE32 (Buf, Buf'First + 12);
      Ctx.H (0) := Ctx.H (0) + (T0 and 16#03FFFFFF#);
      Ctx.H (1) := Ctx.H (1) + (((Interfaces.Shift_Right (T0, 26)
                        or Interfaces.Shift_Left (T1, 6))) and 16#03FFFFFF#);
      Ctx.H (2) := Ctx.H (2) + (((Interfaces.Shift_Right (T1, 20)
                        or Interfaces.Shift_Left (T2, 12))) and 16#03FFFFFF#);
      Ctx.H (3) := Ctx.H (3) + (((Interfaces.Shift_Right (T2, 14)
                        or Interfaces.Shift_Left (T3, 18))) and 16#03FFFFFF#);
      Ctx.H (4) := Ctx.H (4) + (Interfaces.Shift_Right (T3, 8) and 16#03FFFFFF#);
      if Take = 16 then
         Ctx.H (4) := Ctx.H (4) + (1 * 16#1000000#);  --  hibit = 1 << 24
      end if;
      Poly1305_Mul (Ctx.H, Ctx.R0, Ctx.R1, Ctx.R2, Ctx.R3, Ctx.R4,
                    Ctx.S1, Ctx.S2, Ctx.S3, Ctx.S4);
   end Process_Block;

   --  ------------------------------
   --  Set the 32-byte one-time key (r || s) of the context.
   --  ------------------------------
   procedure Set_Key (Ctx   : in out Context;
                      Key32 : in Ada.Streams.Stream_Element_Array) is
   begin
      Ctx.R0 := Load_LE32 (Key32, Key32'First) and 16#03FFFFFF#;
      Ctx.R1 := Interfaces.Shift_Right (Load_LE32 (Key32, Key32'First + 3), 2)
        and 16#03FFFF03#;
      Ctx.R2 := Interfaces.Shift_Right (Load_LE32 (Key32, Key32'First + 6), 4)
        and 16#03FFC0FF#;
      Ctx.R3 := Interfaces.Shift_Right (Load_LE32 (Key32, Key32'First + 9), 6)
        and 16#03F03FFF#;
      Ctx.R4 := Interfaces.Shift_Right (Load_LE32 (Key32, Key32'First + 12), 8)
        and 16#00FFFFF#;
      Ctx.S1 := Ctx.R1 * 5;
      Ctx.S2 := Ctx.R2 * 5;
      Ctx.S3 := Ctx.R3 * 5;
      Ctx.S4 := Ctx.R4 * 5;
      Ctx.SW0 := Load_LE32 (Key32, Key32'First + 16);
      Ctx.SW1 := Load_LE32 (Key32, Key32'First + 20);
      Ctx.SW2 := Load_LE32 (Key32, Key32'First + 24);
      Ctx.SW3 := Load_LE32 (Key32, Key32'First + 28);
      Ctx.H := [others => 0];
      Ctx.Buf := [others => 0];
      Ctx.Buf_Len := 0;
   end Set_Key;

   --  ------------------------------
   --  Update the authenticator with the data.  Full 16-byte blocks are
   --  processed immediately, the trailing partial block is buffered.
   --  ------------------------------
   procedure Update (Ctx  : in out Context;
                     Data : in Ada.Streams.Stream_Element_Array) is
      Pos       : Ada.Streams.Stream_Element_Offset := Data'First;
      Remaining : Natural := Data'Length;
   begin
      if Ctx.Buf_Len > 0 then
         --  Complete the buffered partial block first.
         declare
            Need : constant Natural := 16 - Ctx.Buf_Len;
            Take : constant Natural := Natural'Min (Need, Remaining);
         begin
            for I in 0 .. Take - 1 loop
               Ctx.Buf (Ada.Streams.Stream_Element_Offset (Ctx.Buf_Len + I))
                 := Data (Pos + Ada.Streams.Stream_Element_Offset (I));
            end loop;
            Ctx.Buf_Len := Ctx.Buf_Len + Take;
            Pos := Pos + Ada.Streams.Stream_Element_Offset (Take);
            Remaining := Remaining - Take;
         end;
         if Ctx.Buf_Len = 16 then
            Process_Block (Ctx, Ctx.Buf, 16);
            Ctx.Buf_Len := 0;
         end if;
      end if;
      while Remaining >= 16 loop
         for I in 0 .. 15 loop
            Ctx.Buf (Ada.Streams.Stream_Element_Offset (I))
              := Data (Pos + Ada.Streams.Stream_Element_Offset (I));
         end loop;
         Process_Block (Ctx, Ctx.Buf, 16);
         Pos := Pos + 16;
         Remaining := Remaining - 16;
      end loop;
      if Remaining > 0 then
         for I in 0 .. Remaining - 1 loop
            Ctx.Buf (Ada.Streams.Stream_Element_Offset (I))
              := Data (Pos + Ada.Streams.Stream_Element_Offset (I));
         end loop;
         Ctx.Buf_Len := Remaining;
      end if;
   end Update;

   --  ------------------------------
   --  Finish the computation and return the 128-bit tag in <b>Tag</b>.
   --  ------------------------------
   procedure Finish (Ctx : in out Context;
                     Tag : out Ada.Streams.Stream_Element_Array) is
      Block : Block_Type := [others => 0];
   begin
      if Ctx.Buf_Len > 0 then
         --  Process the last partial block, appending the 2^(8*Take) bit.
         for I in 0 .. Ctx.Buf_Len - 1 loop
            Block (Ada.Streams.Stream_Element_Offset (I))
              := Ctx.Buf (Ada.Streams.Stream_Element_Offset (I));
         end loop;
         Block (Ada.Streams.Stream_Element_Offset (Ctx.Buf_Len)) := 1;
         Process_Block (Ctx, Block, Ctx.Buf_Len);
         Ctx.Buf_Len := 0;
      end if;
      --  Full reduction modulo 2^130 - 5, then add s = key[16..32] mod 2^128.
      declare
         H    : Poly5 := Ctx.H;
         C    : Interfaces.Unsigned_32;
         G    : Poly5;
         Mask : Interfaces.Unsigned_32;
         F0, F1, F2, F3 : Interfaces.Unsigned_32;
         V    : Interfaces.Unsigned_64;
      begin
         C := Interfaces.Shift_Right (H (1), 26); H (1) := H (1) and 16#03FFFFFF#;
         H (2) := H (2) + C;
         C := Interfaces.Shift_Right (H (2), 26); H (2) := H (2) and 16#03FFFFFF#;
         H (3) := H (3) + C;
         C := Interfaces.Shift_Right (H (3), 26); H (3) := H (3) and 16#03FFFFFF#;
         H (4) := H (4) + C;
         C := Interfaces.Shift_Right (H (4), 26); H (4) := H (4) and 16#03FFFFFF#;
         H (0) := H (0) + Interfaces.Unsigned_32 (Interfaces.Unsigned_64 (C) * 5);
         C := Interfaces.Shift_Right (H (0), 26); H (0) := H (0) and 16#03FFFFFF#;
         H (1) := H (1) + C;
         --  g = h + 5 - 2^130; select g when h >= 2^130 - 5
         G (0) := H (0) + 5; C := Interfaces.Shift_Right (G (0), 26);
         G (0) := G (0) and 16#03FFFFFF#;
         G (1) := H (1) + C; C := Interfaces.Shift_Right (G (1), 26);
         G (1) := G (1) and 16#03FFFFFF#;
         G (2) := H (2) + C; C := Interfaces.Shift_Right (G (2), 26);
         G (2) := G (2) and 16#03FFFFFF#;
         G (3) := H (3) + C; C := Interfaces.Shift_Right (G (3), 26);
         G (3) := G (3) and 16#03FFFFFF#;
         G (4) := H (4) + C - (1 * 16#4000000#);  --  subtract 1 << 26 (bit 130)
         Mask := Interfaces.Shift_Right (G (4), 31) - 1;
         for I in 0 .. 4 loop
            H (I) := (H (I) and (not Mask)) or (G (I) and Mask);
         end loop;
         --  pack the 130-bit value into 32-bit words and add s
         F0 := H (0) or Interfaces.Shift_Left (H (1), 26);
         F1 := Interfaces.Shift_Right (H (1), 6) or Interfaces.Shift_Left (H (2), 20);
         F2 := Interfaces.Shift_Right (H (2), 12) or Interfaces.Shift_Left (H (3), 14);
         F3 := Interfaces.Shift_Right (H (3), 18) or Interfaces.Shift_Left (H (4), 8);
         V := Interfaces.Unsigned_64 (F0) + Interfaces.Unsigned_64 (Ctx.SW0);
         F0 := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
         C := Interfaces.Unsigned_32 (Interfaces.Shift_Right (V, 32));
         V := Interfaces.Unsigned_64 (F1) + Interfaces.Unsigned_64 (Ctx.SW1)
           + Interfaces.Unsigned_64 (C);
         F1 := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
         C := Interfaces.Unsigned_32 (Interfaces.Shift_Right (V, 32));
         V := Interfaces.Unsigned_64 (F2) + Interfaces.Unsigned_64 (Ctx.SW2)
           + Interfaces.Unsigned_64 (C);
         F2 := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
         C := Interfaces.Unsigned_32 (Interfaces.Shift_Right (V, 32));
         V := Interfaces.Unsigned_64 (F3) + Interfaces.Unsigned_64 (Ctx.SW3)
           + Interfaces.Unsigned_64 (C);
         F3 := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
         Store_LE32 (F0, Tag, Tag'First);
         Store_LE32 (F1, Tag, Tag'First + 4);
         Store_LE32 (F2, Tag, Tag'First + 8);
         Store_LE32 (F3, Tag, Tag'First + 12);
      end;
   end Finish;

   --  ------------------------------
   --  Compute the Poly1305 tag of a message with a 32-byte one-time key
   --  (r || s) in a single call.
   --  ------------------------------
   procedure Mac (Key32 : in Ada.Streams.Stream_Element_Array;
                  Msg   : in Ada.Streams.Stream_Element_Array;
                  Tag   : out Ada.Streams.Stream_Element_Array) is
      Ctx : Context;
   begin
      Set_Key (Ctx, Key32);
      Update (Ctx, Msg);
      Finish (Ctx, Tag);
   end Mac;

   overriding
   procedure Finalize (Object : in out Context) is
   begin
      Object.H := [others => 0];
      Object.R0 := 0; Object.R1 := 0; Object.R2 := 0; Object.R3 := 0; Object.R4 := 0;
      Object.S1 := 0; Object.S2 := 0; Object.S3 := 0; Object.S4 := 0;
      Object.SW0 := 0; Object.SW1 := 0; Object.SW2 := 0; Object.SW3 := 0;
      Object.Buf := [others => 0];
   end Finalize;

end Util.Encoders.Poly1305;
