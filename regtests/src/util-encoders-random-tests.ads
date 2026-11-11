-----------------------------------------------------------------------
--  util-encoders-random-tests - Random generator tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Util.Encoders.Random.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the generation of random bytes with the default generator.
   procedure Test_Random_Bytes (T : in out Test);

   --  Test the generation of random non-zero bytes.
   procedure Test_Random_Nonzero (T : in out Test);

   --  Test the generation of random strings.
   procedure Test_Random_String (T : in out Test);

   --  Test the generation of base64url tokens.
   procedure Test_Random_Token (T : in out Test);

   --  Test the System_Generator and Pseudo_Generator implementations.
   procedure Test_Random_Generators (T : in out Test);

   --  Test the Random.Secure generic package with a fake incrementing
   --  generator and with the Pseudo_Generator as basis.
   procedure Test_Random_Secure (T : in out Test);

   --  Test the Xorshiftr_Generator (xorshift128+) with reference vectors.
   procedure Test_Xorshiftr (T : in out Test);

   --  Measure the Xorshiftr_Generator generation performance.
   procedure Test_Xorshiftr_Benchmark (T : in out Test);
   procedure Test_System_Benchmark (T : in out Test);
   procedure Test_Pseudo_Benchmark (T : in out Test);
   procedure Test_Secure_Benchmark (T : in out Test);

end Util.Encoders.Random.Tests;
