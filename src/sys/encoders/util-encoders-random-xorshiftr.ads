-----------------------------------------------------------------------
--  util-encoders-random-xorshiftr -- Xorshift128+ pseudo random generator
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Interfaces;

--  === Xorshift+ random generator ===
--  The `Util.Encoders.Random.Xorshiftr` package provides the
--  `Xorshiftr_Generator` type implementing the `Generator` interface with the
--  xorshift128+ algorithm (Sebastiano Vigna, <i>An experimental exploration of
--  Marsaglia's xorshift generators, scrambled</i>, 2014).  The generator has a
--  128-bit state and a period of 2^128 - 1.  It is fast but <b>not</b> suitable
--  for cryptographic use; use `Util.Encoders.Random.Secure` for that.
package Util.Encoders.Random.Xorshiftr with Preelaborate is

   --  The xorshift128+ random generator.
   type Xorshiftr_Generator is limited new Generator with private;

   --  Set the 128-bit generator state directly.  The state must not be zero.
   procedure Set_State (Gen : in out Xorshiftr_Generator;
                        S0  : in     Interfaces.Unsigned_64;
                        S1  : in     Interfaces.Unsigned_64);

   --  Seed the generator from a 64-bit seed.  The seed is expanded to the
   --  128-bit state with the SplitMix64 generator.
   procedure Set_Seed (Gen  : in out Xorshiftr_Generator;
                       Seed : in     Interfaces.Unsigned_64);

   --  Return the next 64-bit pseudo random value.
   function Next (Gen : in out Xorshiftr_Generator) return Interfaces.Unsigned_64;

   --  Fill the array with pseudo random bytes (little-endian 64-bit words).
   overriding
   procedure Generate (Gen  : in out Xorshiftr_Generator;
                       Into : out Ada.Streams.Stream_Element_Array);

private

   type State_Array is array (0 .. 1) of Interfaces.Unsigned_64;

   type Xorshiftr_Generator is limited new Generator with record
      S : State_Array := [1, 2];
   end record;

end Util.Encoders.Random.Xorshiftr;
