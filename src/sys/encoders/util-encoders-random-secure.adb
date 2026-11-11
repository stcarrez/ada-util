-----------------------------------------------------------------------
--  util-encoders-random-secure -- Secure random generation (ChaCha20)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------


package body Util.Encoders.Random.Secure is

   use type Interfaces.Unsigned_32;

   --  ------------------------------
   --  Reseed the generator with a fresh ChaCha20 key and nonce drawn from
   --  the underlying random generator.
   --  ------------------------------
   procedure Reseed (Gen : in out Secure_Generator) is
   begin
      Gen.Rand.Generate (Gen.Key.Secret);
      Gen.Rand.Generate (Gen.Nonce);
      Gen.Counter := 0;
      Gen.Seeded := True;
   end Reseed;

   --  ------------------------------
   --  Fill the array with the ChaCha20 keystream (XOR of the zero buffer with
   --  the keystream), which is cryptographically secure as long as the key
   --  is not reused.
   --  ------------------------------
   overriding
   procedure Generate (Gen  : in out Secure_Generator;
                       Into : out Ada.Streams.Stream_Element_Array) is
      Blocks : constant Interfaces.Unsigned_32
        := (Interfaces.Unsigned_32 (Into'Length) + 63) / 64;
      Zeros  : constant Ada.Streams.Stream_Element_Array (Into'Range) := [others => 0];
   begin
      if not Gen.Seeded then
         Reseed (Gen);
      end if;
      if Gen.Counter > Interfaces.Unsigned_32'Last - Blocks then
         --  The block counter is about to wrap: reseed with a fresh key so
         --  that the keystream is never reused.
         Reseed (Gen);
      end if;
      Util.Encoders.ChaCha20.Crypt (Gen.Key, Gen.Nonce, Gen.Counter, Zeros, Into);
      --  Advance the counter by whole blocks: the keystream of a partially
      --  used block is discarded, never reused.
      Gen.Counter := Gen.Counter + Blocks;
   end Generate;

end Util.Encoders.Random.Secure;
