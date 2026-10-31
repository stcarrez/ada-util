-----------------------------------------------------------------------
--  util-encoders-crc32-tests - CRC32 function tests
--  Copyright (C) 2025-2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams;
with Interfaces;
with Util.Test_Caller;

package body Util.Encoders.CRC32.Tests is

   use Interfaces;

   package Caller is new Util.Test_Caller (Test, "Encoders.CRC32");

   procedure Check_CRC32 (T      : in out Test;
                          Data   : in String;
                          Expect : in Unsigned_32;
                          Title  : in String);

   procedure Check_CRC32 (T      : in out Test;
                          Data   : in Ada.Streams.Stream_Element_Array;
                          Expect : in Unsigned_32;
                          Title  : in String);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.CRC32.Update (Vectors)",
                       Test_CRC32_Vectors'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.CRC32.Update (String)",
                       Test_CRC32_String'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.CRC32.Update (Binary)",
                       Test_CRC32_Binary'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.CRC32.Update (Incremental)",
                       Test_CRC32_Incremental'Access);
   end Add_Tests;

   --  ------------------------------
   --  Compute the CRC32 of a string and check the result.
   --  ------------------------------
   procedure Check_CRC32 (T      : in out Test;
                          Data   : in String;
                          Expect : in Unsigned_32;
                          Title  : in String) is
      E      : Context;
      Result : Digest;
   begin
      E.Init;
      E.Update (Data);
      E.Finish (Result);
      Util.Tests.Assert_Equals (T, Long_Long_Integer (Expect), Long_Long_Integer (Result),
                                Title & ": invalid CRC32 of '" & Data & "'");
   end Check_CRC32;

   --  ------------------------------
   --  Compute the CRC32 of a binary array and check the result.
   --  ------------------------------
   procedure Check_CRC32 (T      : in out Test;
                          Data   : in Ada.Streams.Stream_Element_Array;
                          Expect : in Unsigned_32;
                          Title  : in String) is
      E      : Context;
      Result : Digest;
   begin
      E.Init;
      E.Update (Data);
      E.Finish (Result);
      Util.Tests.Assert_Equals (T, Long_Long_Integer (Expect), Long_Long_Integer (Result),
                                Title & ": invalid CRC32 of binary data");
   end Check_CRC32;

   --  ------------------------------
   --  Test the standard CRC-32 test vectors (CRC-32/ISO-HDLC).
   --  The check value 16#CBF43926# is the CRC-32 of the string "123456789".
   --  ------------------------------
   procedure Test_CRC32_Vectors (T : in out Test) is
   begin
      Check_CRC32 (T, "", 16#0000_0000#, "empty string");
      Check_CRC32 (T, "a", 16#E8B7_BE43#, "a");
      Check_CRC32 (T, "abc", 16#3524_41C2#, "abc");
      Check_CRC32 (T, "123456789", 16#CBF4_3926#, "123456789");
      Check_CRC32 (T, "message digest", 16#2015_9D7F#, "message digest");
      Check_CRC32 (T, "abcdefghijklmnopqrstuvwxyz", 16#4C27_50BD#, "alphabet");
      Check_CRC32 (T, "The quick brown fox jumps over the lazy dog", 16#414F_A339#, "fox");
   end Test_CRC32_Vectors;

   --  ------------------------------
   --  Test crc32 on string
   --  ------------------------------
   procedure Test_CRC32_String (T : in out Test) is
      E      : Context;
      Result : Digest;
   begin
      --  A default context uses the standard CRC-32 polynomial.
      E.Update ("123456789");
      E.Finish (Result);
      Util.Tests.Assert_Equals (T, Long_Long_Integer (16#CBF4_3926#), Long_Long_Integer (Result),
                                "Default context: invalid CRC32 of '123456789'");

      --  Initializing with the standard polynomial gives the same result.
      E.Init (CRC32_Poly);
      E.Update ("123456789");
      E.Finish (Result);
      Util.Tests.Assert_Equals (T, Long_Long_Integer (16#CBF4_3926#), Long_Long_Integer (Result),
                                "Init (CRC32_Poly): invalid CRC32 of '123456789'");
   end Test_CRC32_String;

   --  ------------------------------
   --  Test crc32 on binary
   --  ------------------------------
   procedure Test_CRC32_Binary (T : in out Test) is
      Digits_Data : constant Ada.Streams.Stream_Element_Array (1 .. 9) :=
        (16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#, 16#38#, 16#39#);
      FFs    : constant Ada.Streams.Stream_Element_Array (1 .. 8) := (others => 16#FF#);
      Data   : Ada.Streams.Stream_Element_Array (0 .. 255) := (others => 0);
   begin
      --  "123456789" as a binary array gives the same CRC32 as the string.
      Check_CRC32 (T, Digits_Data, 16#CBF4_3926#, "123456789 binary");

      --  All the byte values 0 .. 255.
      for I in Data'Range loop
         Data (I) := Ada.Streams.Stream_Element (I);
      end loop;
      Check_CRC32 (T, Data, 16#2905_8C73#, "bytes 0..255");

      --  Eight 16#FF# bytes.
      Check_CRC32 (T, FFs, 16#2144_DF1C#, "8 x FF");
   end Test_CRC32_Binary;

   --  ------------------------------
   --  Test crc32 computed with several Update calls.
   --  ------------------------------
   procedure Test_CRC32_Incremental (T : in out Test) is
      E      : Context;
      Result : Digest;
   begin
      E.Init;
      E.Update ("123");
      E.Update ("456");
      E.Update ("789");
      E.Finish (Result);
      Util.Tests.Assert_Equals (T, Long_Long_Integer (16#CBF4_3926#), Long_Long_Integer (Result),
                                "Incremental: invalid CRC32 of '123456789'");

      --  Init resets the context: a new computation starts from scratch.
      E.Init;
      E.Update ("a");
      E.Finish (Result);
      Util.Tests.Assert_Equals (T, Long_Long_Integer (16#E8B7_BE43#), Long_Long_Integer (Result),
                                "Reinit: invalid CRC32 of 'a'");
   end Test_CRC32_Incremental;

end Util.Encoders.CRC32.Tests;
