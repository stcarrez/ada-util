-----------------------------------------------------------------------
--  util-encoders-curve25519 -- Big number arithmetic over GF(2^255 - 19)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Interfaces;

--  Big number arithmetic over the prime field GF(2^255 - 19)
--  This private package provides the big number arithmetic shared by the
--  `Util.Encoders.X25519` (Montgomery curve25519) and `Util.Encoders.Ed25519`
--  (twisted Edwards curve edwards25519) packages.
--  Both curves are defined over the same prime field GF(2^255 - 19).
--
--  A field element is a 256-bit number held as 8 little-endian 32-bit limbs.
--  Values are kept reduced in the range [0, p).
private package Util.Encoders.Curve25519 with Preelaborate is

   use type Interfaces.Unsigned_32;

   --  A 256-bit number held as 8 little-endian 32-bit limbs.
   type Limbs8 is array (0 .. 7) of Interfaces.Unsigned_32;

   --  A 512-bit number held as 17 little-endian 32-bit limbs.
   type Wide17 is array (0 .. 16) of Interfaces.Unsigned_32;

   --  A field element of GF(2^255 - 19), kept reduced in the range [0, p).
   type Fe is new Limbs8;

   --  The field prime p = 2^255 - 19.
   Prime : constant Fe := [0 => 16#FFFFFFED#, 1 .. 6 => 16#FFFFFFFF#, 7 => 16#7FFFFFFF#];

   --  The fixed exponent p - 2 used to compute the inverse of a field element.
   Exp_P_Minus_2 : constant Fe := [0 => 16#FFFFFFEB#, 1 .. 6 => 16#FFFFFFFF#,
                                   7 => 16#7FFFFFFF#];

   Fe_Zero : constant Fe := [others => 0];
   Fe_One  : constant Fe := [0 => 1, others => 0];

   function Fe_Is_Zero (A : Fe) return Boolean is
     ((for all I in A'Range => A (I) = 0));

   --  Build a field element from a 64-bit value.
   function Fe_From_U64 (V : Interfaces.Unsigned_64) return Fe;

   --  Returns True when A >= B.
   function Fe_Ge (A, B : Fe) return Boolean;

   --  If A >= p, returns A - p, otherwise returns A unchanged.
   function Fe_Cond_Sub_P (A : Fe) return Fe;

   --  (A + B) mod p.
   function Fe_Add (A, B : Fe) return Fe;

   --  (A - B) mod p.
   function Fe_Sub (A, B : Fe) return Fe;

   --  Schoolbook 256x256 -> 512 multiplication on 32-bit limbs with a 64-bit
   --  accumulator.
   function Mul_Wide (A, B : Limbs8) return Wide17;

   --  Reduce a 512-bit number modulo p using 2^256 = 38 (mod p).
   function Fe_Reduce_Wide (C : Wide17) return Fe;

   --  (A * B) mod p.
   function Fe_Mul (A, B : Fe) return Fe;

   --  B^E over the field, scanning the exponent bits from the most significant.
   function Fe_Pow (B : Fe; E : Fe) return Fe;

   --  A^(p-2) = A^-1 in the field.
   function Fe_Inv (A : Fe) return Fe;

   --  Decode a 32-byte little-endian field element, clearing the top bit.
   --  The result is not reduced modulo p.
   function Fe_From_Bytes (B  : in Ada.Streams.Stream_Element_Array;
                           Off : Ada.Streams.Stream_Element_Offset) return Fe;

   --  Encode a field element as 32 little-endian bytes (canonical form).
   procedure Fe_To_Bytes (A : in Fe; B : out Ada.Streams.Stream_Element_Array);

end Util.Encoders.Curve25519;
