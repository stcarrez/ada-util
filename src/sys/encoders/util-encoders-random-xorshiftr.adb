-----------------------------------------------------------------------
--  util-encoders-random-xorshiftr -- Xorshift128+ pseudo random generator
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Encoders.Random.Xorshiftr is

   use Interfaces;

   --  ------------------------------
   --  SplitMix64 generator used to expand a 64-bit seed into the 128-bit state.
   --  ------------------------------
   function Splitmix64 (X : in out Unsigned_64) return Unsigned_64 is
      Z : Unsigned_64;
   begin
      X := X + 16#9E37_79B9_7F4A_7C15#;
      Z := X;
      Z := (Z xor Shift_Right (Z, 30)) * 16#BF58_476D_1CE4_E5B9#;
      Z := (Z xor Shift_Right (Z, 27)) * 16#94D0_49BB_1331_11EB#;
      return Z xor Shift_Right (Z, 31);
   end Splitmix64;

   --  ------------------------------
   --  The all-zero state is a fixed point of xorshift, make sure we never use it.
   --  ------------------------------
   procedure Fix_Zero_State (Gen : in out Xorshiftr_Generator) is
   begin
      if Gen.S (0) = 0 and then Gen.S (1) = 0 then
         Gen.S (1) := 1;
      end if;
   end Fix_Zero_State;

   procedure Set_State (Gen : in out Xorshiftr_Generator;
                        S0  : in     Interfaces.Unsigned_64;
                        S1  : in     Interfaces.Unsigned_64) is
   begin
      Gen.S := [S0, S1];
      Fix_Zero_State (Gen);
   end Set_State;

   procedure Set_Seed (Gen  : in out Xorshiftr_Generator;
                       Seed : in     Interfaces.Unsigned_64) is
      X : Unsigned_64 := Seed;
   begin
      Gen.S (0) := Splitmix64 (X);
      Gen.S (1) := Splitmix64 (X);
      Fix_Zero_State (Gen);
   end Set_Seed;

   function Next (Gen : in out Xorshiftr_Generator) return Unsigned_64 is
      S1 : constant Unsigned_64 := Gen.S (0);
      S0 : constant Unsigned_64 := Gen.S (1);
      T  : Unsigned_64;
   begin
      Gen.S (0) := S0;
      T := S1 xor Shift_Left (S1, 23);
      Gen.S (1) := T xor S0 xor Shift_Right (T, 17) xor Shift_Right (S0, 26);
      return Gen.S (1) + S0;
   end Next;

   overriding
   procedure Generate (Gen  : in out Xorshiftr_Generator;
                       Into : out Stream_Element_Array) is
      Size   : constant Stream_Element_Offset := Into'Length / 8;
      Remain : constant Stream_Element_Offset := Into'Length mod 8;
      Value  : Unsigned_64;
   begin
      --  Generate the random sequence (fill 64-bits at a time for each random call).
      for I in 0 .. Size - 1 loop
         Value := Next (Gen);
         for J in 0 .. 7 loop
            Into (Into'First + 8 * I + Stream_Element_Offset (J)) := Stream_Element (Value and 16#FF#);
            Value := Shift_Right (Value, 8);
         end loop;
      end loop;

      --  Fill the remaining bytes.
      if Remain > 0 then
         Value := Next (Gen);
         for I in 0 .. Remain - 1 loop
            Into (Into'First + 8 * Size + I) := Stream_Element (Value and 16#FF#);
            Value := Shift_Right (Value, 8);
         end loop;
      end if;
   end Generate;

end Util.Encoders.Random.Xorshiftr;
