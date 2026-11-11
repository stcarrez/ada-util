-----------------------------------------------------------------------
--  util-encoders-random -- Random bytes and random token generation
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Ada.Strings.Unbounded;

--  == Random Generator ==
--  The `Util.Encoders.Random` package generates random bytes intended to be
--  used for a nonce, a secret key, a seed or an access token.
--
--  The `Generator` interface is implemented by:
--
--  * `Util.Encoders.Random.System.System_Generator`: reads the operating
--    system random source (<tt>/dev/urandom</tt>); when it is not available,
--    a Xorshiftr pseudo-random generator seeded with the clock is used,
--  * `Util.Encoders.Random.Pseudo.Pseudo_Generator`: a pseudo-random
--    generator based on `Ada.Numerics.Discrete_Random` (not suitable
--    for cryptographic use),
--  * `Util.Encoders.Random.Xorshiftr.Xorshiftr_Generator`: a fast
--    xorshift128+ pseudo-random generator (not suitable for cryptographic
--    use),
--  * `Util.Encoders.Random.Synchronized_Generator`: a generic package that
--    wraps a generator in a protected type so that it is safe to share it
--    between several tasks.
--
--  @include util-encoders-random-system.ads
--  @include util-encoders-random-xorshiftr.ads
--  @include util-encoders-random-secure.ads
--  @include util-encoders-random-synchronized_generator.ads
package Util.Encoders.Random with Preelaborate is

   --  Interface of a random generator.
   type Generator is limited interface;

   --  Fill the array with random bytes.
   procedure Generate (Gen  : in out Generator;
                       Into : out Ada.Streams.Stream_Element_Array) is abstract;

   procedure Reseed (Gen : in out Generator) is null;

   --  Fill the array with random non-zero bytes (used for example by the
   --  RSAES-PKCS1-v1_5 padding).
   procedure Generate_Nonzero (Gen  : in out Generator'Class;
                               Into : out Ada.Streams.Stream_Element_Array);

   --  Fill the string with random bytes.
   procedure Generate (Gen  : in out Generator'Class;
                       Into : out String);

   --  Generate a random sequence of <b>Bits</b> bits and return the result
   --  encoded as a base64url string (without the <tt>=</tt> padding).
   function Generate (Gen  : in out Generator'Class;
                      Bits : in Positive) return String;

   --  Generate a random sequence of <b>Bits</b> bits, encode it as a base64url
   --  string and append it to the buffer.
   procedure Generate (Gen  : in out Generator'Class;
                       Bits : in Positive;
                       Into : in out Ada.Strings.Unbounded.Unbounded_String);

end Util.Encoders.Random;
