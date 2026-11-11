-----------------------------------------------------------------------
--  util-encoders-curve25519 -- Big number arithmetic over GF(2^255 - 19)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Encoders.Curve25519 is

   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   function Fe_From_U64 (V : Interfaces.Unsigned_64) return Fe is
     (Fe'[0 => Interfaces.Unsigned_32 (V and 16#FFFFFFFF#),
           1 => Interfaces.Unsigned_32 (Interfaces.Shift_Right (V, 32)),
           others => 0]);

   function Fe_Cond_Sub_P (A : Fe) return Fe is
      R      : Fe;
      Borrow : Interfaces.Unsigned_64 := 0;
   begin
      for I in A'Range loop
         declare
            Ai : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (A (I));
            Pi : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Prime (I));
            V  : Interfaces.Unsigned_64;
         begin
            if Ai >= Pi + Borrow then
               V := Ai - Pi - Borrow;
               Borrow := 0;
            else
               V := Ai + 16#1_0000_0000# - Pi - Borrow;
               Borrow := 1;
            end if;
            R (I) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
         end;
      end loop;
      if Borrow = 0 then
         return R;
      else
         return A;
      end if;
   end Fe_Cond_Sub_P;

   function Fe_Add (A, B : Fe) return Fe is
      R     : Fe;
      Carry : Interfaces.Unsigned_64 := 0;
   begin
      for I in A'Range loop
         declare
            V : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64 (A (I)) + Interfaces.Unsigned_64 (B (I)) + Carry;
         begin
            R (I) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
            Carry := Interfaces.Shift_Right (V, 32);
         end;
      end loop;
      --  A, B < p so A + B < 2p < 2^256: a single conditional subtraction is enough.
      return Fe_Cond_Sub_P (R);
   end Fe_Add;

   function Fe_Sub (A, B : Fe) return Fe is
      R      : Fe;
      Borrow : Interfaces.Unsigned_64 := 0;
   begin
      for I in A'Range loop
         declare
            Ai : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (A (I));
            Bi : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (B (I));
            V  : Interfaces.Unsigned_64;
         begin
            if Ai >= Bi + Borrow then
               V := Ai - Bi - Borrow;
               Borrow := 0;
            else
               V := Ai + 16#1_0000_0000# - Bi - Borrow;
               Borrow := 1;
            end if;
            R (I) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
         end;
      end loop;
      if Borrow = 1 then
         --  Add p back: the discarded carry out is exactly 1 and the result
         --  is A - B + p which lies in (0, 2p).
         declare
            Carry : Interfaces.Unsigned_64 := 0;
         begin
            for I in A'Range loop
               declare
                  V : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64 (R (I))
                      + Interfaces.Unsigned_64 (Prime (I)) + Carry;
               begin
                  R (I) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
                  Carry := Interfaces.Shift_Right (V, 32);
               end;
            end loop;
         end;
      end if;
      return Fe_Cond_Sub_P (R);
   end Fe_Sub;

   --  Schoolbook 256x256 -> 512 multiplication on 32-bit limbs with a 64-bit
   --  accumulator.  The carry stays below 2^32 so each step fits in 64 bits.
   function Mul_Wide (A, B : Limbs8) return Wide17 is
      C : Wide17 := [others => 0];
   begin
      for I in 0 .. 7 loop
         declare
            Carry : Interfaces.Unsigned_64 := 0;
            K     : Natural := I + 8;
         begin
            for J in 0 .. 7 loop
               declare
                  V : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64 (A (I)) * Interfaces.Unsigned_64 (B (J))
                         + Interfaces.Unsigned_64 (C (I + J)) + Carry;
               begin
                  C (I + J) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
                  Carry := Interfaces.Shift_Right (V, 32);
               end;
            end loop;
            while Carry /= 0 loop
               declare
                  V : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64 (C (K)) + Carry;
               begin
                  C (K) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
                  Carry := Interfaces.Shift_Right (V, 32);
                  K := K + 1;
               end;
            end loop;
         end;
      end loop;
      return C;
   end Mul_Wide;


   --  Reduce a 512-bit number modulo p using 2^256 = 38 (mod p).
   function Fe_Reduce_Wide (C : Wide17) return Fe is
      R     : Fe;
      Carry : Interfaces.Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         declare
            V : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64 (C (I))
                   + Interfaces.Unsigned_64 (C (I + 8)) * 38 + Carry;
         begin
            R (I) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
            Carry := Interfaces.Shift_Right (V, 32);
         end;
      end loop;
      while Carry /= 0 loop
         declare
            V : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64 (R (0)) + Carry * 38;
         begin
            R (0) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
            Carry := Interfaces.Shift_Right (V, 32);
         end;
         for I in 1 .. 7 loop
            exit when Carry = 0;
            declare
               V : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64 (R (I)) + Carry;
            begin
               R (I) := Interfaces.Unsigned_32 (V and 16#FFFFFFFF#);
               Carry := Interfaces.Shift_Right (V, 32);
            end;
         end loop;
      end loop;
      --  The value is now below 2^256 = 2p + 38: two conditional subtractions
      --  bring it below p.
      return Fe_Cond_Sub_P (Fe_Cond_Sub_P (R));
   end Fe_Reduce_Wide;

   function Fe_Mul (A, B : Fe) return Fe is
     (Fe_Reduce_Wide (Mul_Wide (Limbs8 (A), Limbs8 (B))));

   --  B^E over the field, scanning the exponent bits from the most significant.
   function Fe_Pow (B : Fe; E : Fe) return Fe is
      R : Fe := Fe_One;
   begin
      for I in reverse 0 .. 255 loop
         R := Fe_Mul (R, R);
         if (E (I / 32) and Interfaces.Shift_Left (Interfaces.Unsigned_32 (1), I mod 32)) /= 0 then
            R := Fe_Mul (R, B);
         end if;
      end loop;
      return R;
   end Fe_Pow;

   --  A^(p-2) = A^-1 in the field.
   function Fe_Inv (A : Fe) return Fe is
     (Fe_Pow (A, Exp_P_Minus_2));

   --  Decode a 32-byte little-endian field element, clearing the top bit.
   --  The result is not reduced modulo p.
   function Fe_From_Bytes (B  : in Ada.Streams.Stream_Element_Array;
                           Off : Ada.Streams.Stream_Element_Offset) return Fe is
      R : Fe;
   begin
      for I in 0 .. 7 loop
         R (I) := Load_LE32 (B, Off + 4 * Ada.Streams.Stream_Element_Offset (I));
      end loop;
      R (7) := R (7) and 16#7FFFFFFF#;
      return R;
   end Fe_From_Bytes;

   --  Encode a field element as 32 little-endian bytes (canonical form).
   procedure Fe_To_Bytes (A : in Fe; B : out Ada.Streams.Stream_Element_Array) is
      V : constant Fe := Fe_Cond_Sub_P (Fe_Cond_Sub_P (A));
   begin
      for I in 0 .. 7 loop
         Store_LE32 (V (I), B, B'First + 4 * Ada.Streams.Stream_Element_Offset (I));
      end loop;
   end Fe_To_Bytes;

end Util.Encoders.Curve25519;
