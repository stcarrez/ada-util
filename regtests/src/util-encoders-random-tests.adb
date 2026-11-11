-----------------------------------------------------------------------
--  util-encoders-random-tests - Random generator tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Interfaces;
with Util.Measures;
with Util.Test_Caller;
with Util.Encoders.ChaCha20;
with Util.Encoders.Random.Pseudo;
with Util.Encoders.Random.Secure;
with Util.Encoders.Random.System;
with Util.Encoders.Random.Xorshiftr;

package body Util.Encoders.Random.Tests is

   use Util.Tests;
   use Util.Encoders.Random.System;

   package Caller is new Util.Test_Caller (Test, "Encoders.Random");
   procedure Check_Base64url (T : in out Test; Token : in String);

   --  A fake random generator that only increments the bytes: its output is
   --  completely predictable (0, 1, 2, ... 255, 0, 1, ...).
   type Increment_Generator is limited new Generator with record
      Counter : Natural := 0;
   end record;

   overriding
   procedure Generate (Gen  : in out Increment_Generator;
                       Into : out Stream_Element_Array);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.Generate (bytes)",
                       Test_Random_Bytes'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.Generate_Nonzero",
                       Test_Random_Nonzero'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.Generate (string)",
                       Test_Random_String'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.Generate (token)",
                       Test_Random_Token'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random generators",
                       Test_Random_Generators'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.Secure",
                       Test_Random_Secure'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.Xorshiftr",
                       Test_Xorshiftr'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.Xorshiftr benchmark",
                       Test_Xorshiftr_Benchmark'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.System benchmark",
                       Test_System_Benchmark'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.Pseudo benchmark",
                       Test_Pseudo_Benchmark'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Random.Secure benchmark",
                       Test_Secure_Benchmark'Access);
   end Add_Tests;

   --  Check that the token only contains base64url characters.
   procedure Check_Base64url (T : in out Test; Token : in String) is
   begin
      for C of Token loop
         T.Assert ((C in 'A' .. 'Z') or else (C in 'a' .. 'z')
                   or else (C in '0' .. '9') or else C = '-' or else C = '_',
                   "Invalid base64url character in token: " & Token);
      end loop;
   end Check_Base64url;

   --  ------------------------------
   --  Test the generation of random bytes with the default generator.
   --  ------------------------------
   procedure Test_Random_Bytes (T : in out Test) is
      Rand : Util.Encoders.Random.Pseudo.Pseudo_Generator;
      B1, B2 : Stream_Element_Array (1 .. 32);
      All_Zero : Boolean;
   begin
      Rand.Generate (B1);
      Rand.Generate (B2);
      T.Assert (B1 /= B2, "Two random arrays are identical");
      All_Zero := True;
      for B of B1 loop
         if B /= 0 then
            All_Zero := False;
         end if;
      end loop;
      T.Assert (not All_Zero, "Random array is all zeros");
   end Test_Random_Bytes;

   --  ------------------------------
   --  Test the generation of random non-zero bytes.
   --  ------------------------------
   procedure Test_Random_Nonzero (T : in out Test) is
      Rand : Util.Encoders.Random.Pseudo.Pseudo_Generator;
      B : Stream_Element_Array (1 .. 256);
   begin
      Rand.Generate_Nonzero (B);
      for I in B'Range loop
         T.Assert (B (I) /= 0, "Generate_Nonzero produced a zero byte");
      end loop;
   end Test_Random_Nonzero;

   --  ------------------------------
   --  Test the generation of random strings.
   --  ------------------------------
   procedure Test_Random_String (T : in out Test) is
      Rand : Util.Encoders.Random.Pseudo.Pseudo_Generator;
      S1, S2 : String (1 .. 16);
   begin
      Rand.Generate (S1);
      Rand.Generate (S2);
      T.Assert (S1 /= S2, "Two random strings are identical");
   end Test_Random_String;

   --  ------------------------------
   --  Test the generation of base64url tokens.
   --  ------------------------------
   procedure Test_Random_Token (T : in out Test) is
      Rand : Util.Encoders.Random.Pseudo.Pseudo_Generator;
      Tok1 : constant String := Rand.Generate (128);
      Tok2 : constant String := Rand.Generate (256);
      Buf  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  128 bits = 16 random bytes -> 22 base64url chars (24 minus '==' padding).
      Assert_Equals (T, 22, Integer (Tok1'Length), "Invalid token length for 128 bits");
      --  256 bits = 32 random bytes -> 43 base64url chars (44 minus '=' padding).
      Assert_Equals (T, 43, Integer (Tok2'Length), "Invalid token length for 256 bits");
      Check_Base64url (T, Tok1);
      Check_Base64url (T, Tok2);
      T.Assert (Tok1 /= Rand.Generate (128), "Two 128-bit tokens are identical");

      Util.Encoders.Random.Generate (Rand, 128, Buf);
      Assert_Equals (T, 22, Integer (Ada.Strings.Unbounded.Length (Buf)),
                     "Invalid appended token length");
   end Test_Random_Token;

   --  ------------------------------
   --  Test the System_Generator and Pseudo_Generator implementations.
   --  ------------------------------
   procedure Test_Random_Generators (T : in out Test) is
      Sys  : System_Generator;
      Pse  : Util.Encoders.Random.Pseudo.Pseudo_Generator;
      B1   : Stream_Element_Array (1 .. 32);
      B2   : Stream_Element_Array (1 .. 32);
      B3   : Stream_Element_Array (1 .. 32);
   begin
      Sys.Generate (B1);
      Pse.Generate (B2);
      Sys.Generate (B3);
      T.Assert (B1 /= B2, "System and pseudo generators gave the same bytes");
      T.Assert (B1 /= B3, "Two system generator calls gave the same bytes");

      declare
         Tok : constant String := Util.Encoders.Random.Generate (Sys, 256);
      begin
         T.Assert (Tok'Length = 43, "System token length is" & Natural'Image (Tok'Length));
         Check_Base64url (T, Tok);
      end;
      declare
         Tok : constant String := Util.Encoders.Random.Generate (Pse, 128);
      begin
         T.Assert (Tok'Length = 22, "Pseudo token length is" & Natural'Image (Tok'Length));
         Check_Base64url (T, Tok);
      end;
   end Test_Random_Generators;

   overriding
   procedure Generate (Gen  : in out Increment_Generator;
                       Into : out Stream_Element_Array) is
   begin
      for I in Into'Range loop
         Into (I) := Stream_Element (Gen.Counter mod 256);
         Gen.Counter := Gen.Counter + 1;
      end loop;
   end Generate;

   --  ------------------------------
   --  Test the Random.Secure generic package: the ChaCha20 keystream seeded
   --  from the underlying generator must convert even a trivial incrementing
   --  generator into a cryptographically secure one.
   --  ------------------------------
   procedure Test_Random_Secure (T : in out Test) is
      package Secure_Increment is
        new Util.Encoders.Random.Secure (Random_Generator => Increment_Generator);
      package Secure_Pseudo is
        new Util.Encoders.Random.Secure
          (Random_Generator => Util.Encoders.Random.Pseudo.Pseudo_Generator);

      Hex_Enc : constant Util.Encoders.Encoder := Util.Encoders.Create ("hex");

      --  The fake generator produces 0, 1, 2, ... so after the reseed the
      --  ChaCha20 key is bytes 0..31 and the nonce is bytes 32..43.
      Key    : Util.Encoders.Secret_Key (Length => Util.Encoders.ChaCha20.Key_Size);
      Nonce  : Util.Encoders.ChaCha20.Nonce;
      Block0 : Stream_Element_Array (1 .. Util.Encoders.ChaCha20.Block_Size);
      Block1 : Stream_Element_Array (1 .. Util.Encoders.ChaCha20.Block_Size);
      Buf    : Stream_Element_Array (1 .. 32);
      Fake   : Stream_Element_Array (1 .. 32);
      Gen    : Secure_Increment.Secure_Generator;
   begin
      for I in Key.Secret'Range loop
         Key.Secret (I) := Stream_Element (I - Key.Secret'First);
      end loop;
      for I in Nonce'Range loop
         Nonce (I) := Stream_Element (32 + (I - Nonce'First));
      end loop;
      for I in Fake'Range loop
         Fake (I) := Stream_Element (I - Fake'First);
      end loop;
      Util.Encoders.ChaCha20.Block (Key, 0, Nonce, Block0);
      Util.Encoders.ChaCha20.Block (Key, 1, Nonce, Block1);

      --  First Generate: reseeds (key = 0..31, nonce = 32..43) and emits
      --  the first 32 bytes of the ChaCha20 keystream block 0.
      Gen.Generate (Buf);
      Assert_Equals (T, Hex_Enc.Encode_Binary (Block0 (1 .. 32)),
                     Hex_Enc.Encode_Binary (Buf),
                     "Secure generator did not emit the ChaCha20 keystream");
      T.Assert (Buf /= Fake, "Secure generator forwarded the predictable bytes");

      --  Second Generate: the block counter advanced to 1.
      Gen.Generate (Buf);
      Assert_Equals (T, Hex_Enc.Encode_Binary (Block1 (1 .. 32)),
                     Hex_Enc.Encode_Binary (Buf),
                     "Secure generator did not continue the ChaCha20 keystream");

      --  With the pseudo generator as basis, the output must look random:
      --  not all zero and two calls give different bytes.
      declare
         Pse : Secure_Pseudo.Secure_Generator;
         B1, B2 : Stream_Element_Array (1 .. 32);
      begin
         Pse.Generate (B1);
         Pse.Generate (B2);
         T.Assert ((for some B of B1 => B /= 0), "Secure(pseudo) output is all zeros");
         T.Assert (B1 /= B2, "Two Secure(pseudo) calls gave the same bytes");
      end;
   end Test_Random_Secure;

   --  ------------------------------
   --  Test the Xorshiftr_Generator (xorshift128+) against reference vectors
   --  computed with an independent implementation of the algorithm.
   --  ------------------------------
   procedure Test_Xorshiftr (T : in out Test) is
      use Interfaces;
      Gen : Util.Encoders.Random.Xorshiftr.Xorshiftr_Generator;

      procedure Check (Expected : in Unsigned_64) is
         Got : constant Unsigned_64 := Gen.Next;
      begin
         T.Assert (Got = Expected,
                   "Invalid xorshift128+ value, got" & Unsigned_64'Image (Got)
                   & " expected" & Unsigned_64'Image (Expected));
      end Check;
   begin
      --  Reference vector with the state (1, 2).
      Gen.Set_State (1, 2);
      Check (16#800045#);
      Check (16#2000104#);
      Check (16#4000020010C3#);
      Check (16#C00002103045#);

      --  Reference vectors with a 64-bit seed expanded by SplitMix64.
      Gen.Set_Seed (1);
      Check (16#9890A08CC61D98A5#);
      Check (16#91A182125B3DFDC4#);
      Check (16#D3E5DBE3CB93DD47#);
      Check (16#5B51B331603C7565#);
      Gen.Set_Seed (42);
      Check (16#AF1F56FC41A4D2D2#);

      --  Generate fills the array with little-endian 64-bit words.
      Gen.Set_State (1, 2);
      declare
         Buf      : Stream_Element_Array (1 .. 20);
         Expected : constant Stream_Element_Array (1 .. 20) :=
           [16#45#, 16#00#, 16#80#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
            16#04#, 16#01#, 16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#,
            16#C3#, 16#10#, 16#00#, 16#02#];
      begin
         Gen.Generate (Buf);
         T.Assert (Buf = Expected, "Invalid xorshift128+ generated bytes");
      end;

      --  The all-zero state is a fixed point and must be corrected.
      Gen.Set_State (0, 0);
      T.Assert (Gen.Next /= 0, "Xorshift128+ is stuck on the zero state");
   end Test_Xorshiftr;

   procedure Benchmark (Gen   : in out Generator'Class;
                        Title : in String) is
      Buf : Stream_Element_Array (1 .. 1_024);
      S : Util.Measures.Stamp;
   begin
      for I in 1 .. 10_000 loop
         Gen.Generate (Buf);
      end loop;
      Util.Measures.Report (S, Title, Count => 10_000);
   end Benchmark;

   --  ------------------------------
   --  Measure the Xorshiftr_Generator generation performance.
   --  ------------------------------
   procedure Test_Xorshiftr_Benchmark (T : in out Test) is
      pragma Unreferenced (T);
      Gen : Util.Encoders.Random.Xorshiftr.Xorshiftr_Generator;
   begin
      Gen.Set_Seed (42);
      Benchmark (Gen, "Random.Xorshift128+ generate 1024 bytes");
   end Test_Xorshiftr_Benchmark;

   procedure Test_System_Benchmark (T : in out Test) is
      pragma Unreferenced (T);
      Gen : Util.Encoders.Random.System.System_Generator;
   begin
      Benchmark (Gen, "Random.System generate 1024 bytes");
   end Test_System_Benchmark;

   procedure Test_Pseudo_Benchmark (T : in out Test) is
      pragma Unreferenced (T);
      Gen : Util.Encoders.Random.Pseudo.Pseudo_Generator;
   begin
      Benchmark (Gen, "Random.Pseudo generate 1024 bytes");
   end Test_Pseudo_Benchmark;

   procedure Test_Secure_Benchmark (T : in out Test) is
      pragma Unreferenced (T);
      package Secure_Pseudo is
        new Util.Encoders.Random.Secure
          (Random_Generator => Util.Encoders.Random.Pseudo.Pseudo_Generator);

      Gen : Secure_Pseudo.Secure_Generator;
   begin
      Benchmark (Gen, "Random.Secure generate 1024 bytes");
   end Test_Secure_Benchmark;

end Util.Encoders.Random.Tests;
