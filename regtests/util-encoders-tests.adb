-----------------------------------------------------------------------
--  util-encodes-tests - Test for encoding
--  Copyright (C) 2009 - 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Measures;
with Util.Strings.Transforms;
with Ada.Text_IO;
with Util.Encoders.SHA1;
with Util.Encoders.SHA256;
with Util.Encoders.HMAC.SHA1;
with Util.Encoders.HMAC.SHA256;
with Util.Encoders.Base16;
with Util.Encoders.Base64;
with Util.Encoders.AES;
with Util.Encoders.Quoted_Printable;
with Util.Encoders.URI;
package body Util.Encoders.Tests is

   use Util.Tests;

   procedure Check_HMAC (T      : in out Test'Class;
                         Key    : in String;
                         Value  : in String;
                         Expect : in String);
   procedure Check_HMAC256 (T      : in out Test'Class;
                            Key    : in String;
                            Value  : in String;
                            Expect : in String);

   package Caller is new Util.Test_Caller (Test, "Encoders");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.Base16.Encode",
                       Test_Hex'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base16.Decode",
                       Test_Hex'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base64.Encode",
                       Test_Base64_Encode'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base64.Decode",
                       Test_Base64_Decode'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base64.Encode (URL)",
                       Test_Base64_URL_Encode'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base64.Decode (URL)",
                       Test_Base64_URL_Decode'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base64.Benchmark",
                       Test_Base64_Benchmark'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.SHA1.Encode",
                       Test_SHA1_Encode'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.SHA1.Benchmark",
                       Test_SHA1_Benchmark'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.SHA256.Encode",
                       Test_SHA256_Encode'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA1.Sign_SHA1 (RFC2202 test1)",
                       Test_HMAC_SHA1_RFC2202_T1'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA1.Sign_SHA1 (RFC2202 test2)",
                       Test_HMAC_SHA1_RFC2202_T2'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA1.Sign_SHA1 (RFC2202 test3)",
                       Test_HMAC_SHA1_RFC2202_T3'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA1.Sign_SHA1 (RFC2202 test4)",
                       Test_HMAC_SHA1_RFC2202_T4'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA1.Sign_SHA1 (RFC2202 test5)",
                       Test_HMAC_SHA1_RFC2202_T5'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA1.Sign_SHA1 (RFC2202 test6)",
                       Test_HMAC_SHA1_RFC2202_T6'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA1.Sign_SHA1 (RFC2202 test7)",
                       Test_HMAC_SHA1_RFC2202_T7'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Encode_LEB128",
                       Test_LEB128'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base64.Encode",
                       Test_Base64_LEB128'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA256.Sign_SHA1 (RFC4231 test1)",
                       Test_HMAC_SHA256_RFC4231_T1'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA256.Sign_SHA1 (RFC4231 test2)",
                       Test_HMAC_SHA256_RFC4231_T2'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA256.Sign_SHA1 (RFC4231 test3)",
                       Test_HMAC_SHA256_RFC4231_T3'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA256.Sign_SHA1 (RFC4231 test4)",
                       Test_HMAC_SHA256_RFC4231_T4'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA256.Sign_SHA1 (RFC4231 test5)",
                       Test_HMAC_SHA256_RFC4231_T5'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA256.Sign_SHA1 (RFC4231 test6)",
                       Test_HMAC_SHA256_RFC4231_T6'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.HMAC.SHA256.Sign_SHA1 (RFC4231 test7)",
                       Test_HMAC_SHA256_RFC4231_T7'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.AES.Encrypt",
                       Test_AES'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.AES.Encrypt_Secret (CBC)",
                       Test_Encrypt_Decrypt_Secret'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.AES.Encrypt_Secret (CFB)",
                       Test_Encrypt_Decrypt_Secret_CFB'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.AES.Encrypt_Secret (OFB)",
                       Test_Encrypt_Decrypt_Secret_OFB'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.AES.Encrypt_Secret (CTR)",
                       Test_Encrypt_Decrypt_Secret_CTR'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Quoted_Printable.Decode",
                       Test_Decode_Quoted_Printable'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.URI.Encode",
                       Test_Encode_URI'Access);
   end Add_Tests;

   procedure Test_Base64_Encode (T : in out Test) is
      C : constant Util.Encoders.Encoder := Create ("base64");
      D : constant Util.Encoders.Decoder := Create ("base64");
   begin
      Assert_Equals (T, "YQ==", Util.Encoders.Encode (C, "a"));
      Assert_Equals (T, "fA==", Util.Encoders.Encode (C, "|"));
      Assert_Equals (T, "fHw=", Util.Encoders.Encode (C, "||"));
      Assert_Equals (T, "fH5+", Util.Encoders.Encode (C, "|~~"));
      Test_Encoder (T, C, D);
   end Test_Base64_Encode;

   procedure Test_Base64_Decode (T : in out Test) is
      C : constant Util.Encoders.Encoder := Create ("base64");
      D : constant Util.Encoders.Decoder := Create ("base64");
   begin
      Assert_Equals (T, "a", Util.Encoders.Decode (D, "YQ=="));
      Assert_Equals (T, "|", Util.Encoders.Decode (D, "fA=="));
      Assert_Equals (T, "||", Util.Encoders.Decode (D, "fHw="));
      Assert_Equals (T, "|~~", Util.Encoders.Decode (D, "fH5+"));
      Test_Encoder (T, C, D);
   end Test_Base64_Decode;

   procedure Test_Base64_URL_Encode (T : in out Test) is
      C : constant Util.Encoders.Encoder := Create ("base64url");
      D : constant Util.Encoders.Decoder := Create ("base64url");
   begin
      Assert_Equals (T, "YQ==", Util.Encoders.Encode (C, "a"));
      Assert_Equals (T, "fA==", Util.Encoders.Encode (C, "|"));
      Assert_Equals (T, "fHw=", Util.Encoders.Encode (C, "||"));
      Assert_Equals (T, "fH5-", Util.Encoders.Encode (C, "|~~"));
      Assert_Equals (T, "fH5_", Util.Encoders.Encode (C, "|~" & ASCII.DEL));
      Test_Encoder (T, C, D);
   end Test_Base64_URL_Encode;

   procedure Test_Base64_URL_Decode (T : in out Test) is
      C : constant Util.Encoders.Encoder := Create ("base64url");
      D : constant Util.Encoders.Decoder := Create ("base64url");
   begin
      Assert_Equals (T, "a", Util.Encoders.Decode (D, "YQ=="));
      Assert_Equals (T, "|", Util.Encoders.Decode (D, "fA=="));
      Assert_Equals (T, "||", Util.Encoders.Decode (D, "fHw="));
      Assert_Equals (T, "|~~", Util.Encoders.Decode (D, "fH5-"));
      Assert_Equals (T, "|~" & ASCII.DEL, Util.Encoders.Decode (D, "fH5_"));
      Test_Encoder (T, C, D);
   end Test_Base64_URL_Decode;

   procedure Test_Encoder (T : in out Test;
                           C : in Util.Encoders.Encoder;
                           D : in Util.Encoders.Decoder) is
   begin
      for I in 1 .. 334 loop
         declare
            Pattern : String (1 .. I);
         begin
            for J in Pattern'Range loop
               Pattern (J) := Character'Val (((J + I) mod 63) + 32);
            end loop;
            declare
               E : constant String := Util.Encoders.Encode (C, Pattern);
               R : constant String := Util.Encoders.Decode (D, E);
            begin
               Assert_Equals (T, Pattern, R, "Encoding failed for length "
                              & Integer'Image (I) & " code: " & E);
            end;
         exception
            when others =>
               Ada.Text_IO.Put_Line ("Error at index " & Integer'Image (I));
               raise;
         end;
      end loop;
   end Test_Encoder;

   procedure Test_Hex (T : in out Test) is
      C : constant Util.Encoders.Encoder := Create ("hex");
      D : constant Util.Encoders.Decoder := Create ("hex");
   begin
      Assert_Equals (T, "41424344", Util.Encoders.Encode (C, "ABCD"));
      Assert_Equals (T, "ABCD", Util.Encoders.Decode (D, "41424344"));
      Test_Encoder (T, C, D);

      Assert_Equals (T, "ABCD", C.Encode_Unsigned_16 (16#abcd#));
      Assert_Equals (T, "12345678", C.Encode_Unsigned_32 (16#12345678#));
      Assert_Equals (T, "0000ABCD12345678", C.Encode_Unsigned_64 (16#abcd12345678#));
   end Test_Hex;

   procedure Test_Base64_Benchmark (T : in out Test) is
      pragma Unreferenced (T);

      C : constant Util.Encoders.Encoder := Create ("base64");
      S : constant String (1 .. 1_024) := (others => 'a');
   begin
      declare
         T : Util.Measures.Stamp;
         R : constant String := Util.Encoders.Encode (C, S);
         pragma Unreferenced (R);
      begin
         Util.Measures.Report (T, "Base64 encode 1024 bytes");
      end;
   end Test_Base64_Benchmark;

   procedure Test_SHA1_Encode (T : in out Test) is
      procedure Check_Hash (Value  : in String;
                            Expect : in String);

      C    : Util.Encoders.SHA1.Context;
      Hash : Util.Encoders.SHA1.Digest;

      procedure Check_Hash (Value  : in String;
                            Expect : in String) is
         J, N : Natural;
         Ctx  : Util.Encoders.SHA1.Context;
      begin
         for I in 1 .. Value'Length loop
            J := Value'First;
            while J <= Value'Last loop
               if J + I <= Value'Last then
                  N := J + I;
               else
                  N := Value'Last;
               end if;
               Util.Encoders.SHA1.Update (Ctx, Value (J .. N));
               J := N + 1;
            end loop;
            Util.Encoders.SHA1.Finish (Ctx, Hash);
            Assert_Equals (T, Expect, Hash, "Invalid hash for: " & Value);
         end loop;
      end Check_Hash;

      Hex_Decoder : Util.Encoders.Base16.Decoder;
      Last        : Ada.Streams.Stream_Element_Offset;
      Sign        : Util.Encoders.SHA1.Hash_Array;
   begin
      Util.Encoders.SHA1.Update (C, "a");
      Util.Encoders.SHA1.Finish (C, Hash);

      Assert_Equals (T, "86F7E437FAA5A7FCE15D1DDCB9EAEAEA377667B8", Hash,
                     "Invalid hash for 'a'");

      Check_Hash ("ut", "E746699D3947443D84DAD1E0C58BF7AD34712438");
      Check_Hash ("Uti", "2C669751BDC4929377245F5EEBEAED1CE4DA8A45");
      Check_Hash ("Util", "4C31156EFED35EE7814650F8971C3698059440E3");
      Check_Hash ("Util.Encoders", "7DB6007AD8BAEA7C167FF2AE06C9F50A4645F971");
      Check_Hash ("e746699d3947443d84dad1e0c58bf7ad347124382C669751BDC492937"
                  & "7245F5EEBEAED1CE4DA8A45",
                  "875C9C0DE4CE91ED8F432DD02B5BB40CD35DAACD");

      Util.Encoders.Transform (E       => Hex_Decoder,
                               Data    => "D803BA2155CD12D8997117E0846AD2D4555BEB28",
                               Into    => Sign,
                               Last    => Last);
      Assert_Equals (T, Natural (Sign'Last), Natural (Last), "Decoding SHA1 failed");

      Util.Encoders.Transform (E       => Hex_Decoder,
                               Data    => "D803BA2155CD12D8997117E0846AD2D4555BEB",
                               Into    => Sign,
                               Last    => Last);
      Assert_Equals (T, Natural (Sign'Last) - 1, Natural (Last), "Decoding SHA1 failed");

      begin
         Util.Encoders.Transform (E       => Hex_Decoder,
                                  Data    => "D803BA2155CD12D8997117E0846AD2D4555BEB2801",
                                  Into    => Sign,
                                  Last    => Last);
         Fail (T, "No Encoding_Error exception raised");
      exception
         when Encoding_Error =>
            null;
      end;
   end Test_SHA1_Encode;

   --  ------------------------------
   --  Benchmark test for SHA1
   --  ------------------------------
   procedure Test_SHA1_Benchmark (T : in out Test) is
      pragma Unreferenced (T);

      Hash  : Util.Encoders.SHA1.Digest;
      Sizes : constant array (1 .. 6) of Positive := (1, 10, 100, 1000, 10000, 100000);
   begin
      for I in Sizes'Range loop
         declare
            Size : constant Positive := Sizes (I);
            S    : constant String (1 .. Size) := (others => '0');
            T1   : Util.Measures.Stamp;
            C    : Util.Encoders.SHA1.Context;
         begin
            Util.Encoders.SHA1.Update (C, S);
            Util.Encoders.SHA1.Finish (C, Hash);

            Util.Measures.Report (T1, "Encode SHA1" & Integer'Image (Size) & " bytes");
         end;
      end loop;
   end Test_SHA1_Benchmark;

   procedure Test_SHA256_Encode (T : in out Test) is
      procedure Check_Hash (Value  : in String;
                            Expect : in String);

      C      : Util.Encoders.SHA256.Context;
      Hash   : Util.Encoders.SHA256.Digest;
      Digest : Util.Encoders.SHA256.Base64_Digest;

      procedure Check_Hash (Value  : in String;
                            Expect : in String) is
         J, N : Natural;
         Ctx  : Util.Encoders.SHA256.Context;
      begin
         for I in 1 .. Value'Length loop
            J := Value'First;
            while J <= Value'Last loop
               if J + I <= Value'Last then
                  N := J + I;
               else
                  N := Value'Last;
               end if;
               Util.Encoders.SHA256.Update (Ctx, Value (J .. N));
               J := N + 1;
            end loop;
            Util.Encoders.SHA256.Finish_Base64 (Ctx, Digest);
            Ada.Text_IO.Put_Line ("Pass " & Natural'Image (Natural (I)) & ": " & Digest);
            Assert_Equals (T, Expect, Digest, "Invalid SHA256-base64 digest for: " & Value);
         end loop;
      end Check_Hash;

   begin
      Util.Encoders.SHA256.Update (C, "a");
      Util.Encoders.SHA256.Finish (C, Hash);

      Assert_Equals (T, "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb",
                     Hash,
                     "Invalid hash for 'a'");

      Check_Hash ("ut", "RpzGQ6Gft1kB5dMxuNwUvvqLmIELJGwvsi//DDgtw54=");
      Check_Hash ("Uti", "cgM7MSIWSqV9OkdvuTPRKGlB3MWUVIaDLsZs/+7Wucs=");
      Check_Hash ("Util", "+29Ow/TX6KR5mTjGr98WyuMeizmNl4g5XGZzpDYOn3A=");
      Check_Hash ("Util.Encoders", "fzUVgu2+6QAfbf/CLYJeDFoeGTm7CHivxiFEUm0K80E=");
      Check_Hash ("e746699d3947443d84dad1e0c58bf7ad347124382C669751BDC492937"
                  & "7245F5EEBEAED1CE4DA8A45",
                  "ZGrFiFCpzbCzN8xfRoVd5VmMlRU7PDMAPZRN34GaAJo=");
   end Test_SHA256_Encode;

   procedure Check_HMAC (T      : in out Test'Class;
                         Key    : in String;
                         Value  : in String;
                         Expect : in String) is
      H : constant String := Util.Encoders.HMAC.SHA1.Sign (Key, Value);
   begin
      Assert_Equals (T, Expect, Util.Strings.Transforms.To_Lower_Case (H),
                     "Invalid HMAC-SHA1");
   end Check_HMAC;

   procedure Check_HMAC256 (T      : in out Test'Class;
                            Key    : in String;
                            Value  : in String;
                            Expect : in String) is
      H : constant String := Util.Encoders.HMAC.SHA256.Sign (Key, Value);
      B : constant Util.Encoders.SHA256.Hash_Array := Util.Encoders.HMAC.SHA256.Sign (Key, Value);
      C : constant Encoders.SHA256.Base64_Digest := Encoders.HMAC.SHA256.Sign_Base64 (Key, Value);
      B16 : constant Encoder := Create ("hex");
      B64 : constant Decoder := Create ("base64");
      B2 : constant Ada.Streams.Stream_Element_Array := B;
      S  : constant String := B16.Encode_Binary (B2);
      B3 : constant Util.Encoders.SHA256.Hash_Array := B64.Decode_Binary (C);
   begin
      Assert_Equals (T, Expect, Util.Strings.Transforms.To_Lower_Case (H),
                     "Invalid HMAC-SHA256");
      Assert_Equals (T, H, S, "Invalid HMAC-SHA256 binary");
      if B /= B3 then
         Ada.Text_IO.Put_Line ("Invalid binary");
      end if;
      Assert_Equals (T, H, B16.Encode_Binary (B3), "Invalid HMAC-SHA256 base64");
   end Check_HMAC256;

   --  ------------------------------
   --  Test HMAC-SHA1
   --  ------------------------------
   procedure Test_HMAC_SHA1_RFC2202_T1 (T : in out Test) is
      Key : constant String (1 .. 20) := (others => Character'Val (16#0b#));
   begin
      Check_HMAC (T, Key, "Hi There", "b617318655057264e28bc0b6fb378c8ef146be00");
   end Test_HMAC_SHA1_RFC2202_T1;

   procedure Test_HMAC_SHA1_RFC2202_T2 (T : in out Test) is
   begin
      Check_HMAC (T, "Jefe", "what do ya want for nothing?",
                  "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79");
   end Test_HMAC_SHA1_RFC2202_T2;

   procedure Test_HMAC_SHA1_RFC2202_T3 (T : in out Test) is
      Key  : constant String (1 .. 20) := (others => Character'Val (16#aa#));
      Data : constant String (1 .. 50) := (others => Character'Val (16#dd#));
   begin
      Check_HMAC (T, Key, Data,
                  "125d7342b9ac11cd91a39af48aa17b4f63f175d3");
   end Test_HMAC_SHA1_RFC2202_T3;

   procedure Test_HMAC_SHA1_RFC2202_T4 (T : in out Test) is
      C    : constant Util.Encoders.Decoder := Create ("hex");
      Key  : constant String := Util.Encoders.Decode (C, "0102030405060708090a0b0c0d0e0f"
                                                      & "10111213141516171819");
      Data : constant String (1 .. 50) := (others => Character'Val (16#cd#));
   begin
      Check_HMAC (T, Key, Data,
                  "4c9007f4026250c6bc8414f9bf50c86c2d7235da");
   end Test_HMAC_SHA1_RFC2202_T4;

   procedure Test_HMAC_SHA1_RFC2202_T5 (T : in out Test) is
      Key  : constant String (1 .. 20) := (others => Character'Val (16#0c#));
   begin
      --  RFC2202 test case 5 but without truncation...
      Check_HMAC (T, Key, "Test With Truncation",
                  "4c1a03424b55e07fe7f27be1d58bb9324a9a5a04");
   end Test_HMAC_SHA1_RFC2202_T5;

   procedure Test_HMAC_SHA1_RFC2202_T6 (T : in out Test) is
      Key  : constant String (1 .. 80) := (others => Character'Val (16#aa#));
   begin
      Check_HMAC (T, Key, "Test Using Larger Than Block-Size Key - Hash Key First",
                  "aa4ae5e15272d00e95705637ce8a3b55ed402112");
   end Test_HMAC_SHA1_RFC2202_T6;

   procedure Test_HMAC_SHA1_RFC2202_T7 (T : in out Test) is
      Key  : constant String (1 .. 80) := (others => Character'Val (16#Aa#));
   begin
      Check_HMAC (T, Key, "Test Using Larger Than Block-Size Key and Larger "
                    & "Than One Block-Size Data",
                  "e8e99d0f45237d786d6bbaa7965c7808bbff1a91");
   end Test_HMAC_SHA1_RFC2202_T7;

   --  ------------------------------
   --  Test encoding leb128.
   --  ------------------------------
   procedure Test_LEB128 (T : in out Test) is
      use type Interfaces.Unsigned_64;

      Data : Ada.Streams.Stream_Element_Array (1 .. 100);
      Last : Ada.Streams.Stream_Element_Offset;
      Val  : Interfaces.Unsigned_64;
      Res  : Interfaces.Unsigned_64;
   begin
      Encode_LEB128 (Into => Data,
                     Pos  => Data'First,
                     Val  => 1,
                     Last => Last);
      Util.Tests.Assert_Equals (T, 1, Integer (Last), "Invalid last position");
      Util.Tests.Assert_Equals (T, 1, Integer (Data (1)), "Invalid value");

      Encode_LEB128 (Into => Data,
                     Pos  => Data'First,
                     Val  => 16#80#,
                     Last => Last);
      Util.Tests.Assert_Equals (T, 2, Integer (Last), "Invalid last position");
      Util.Tests.Assert_Equals (T, 16#80#, Integer (Data (1)), "Invalid value");
      Util.Tests.Assert_Equals (T, 16#01#, Integer (Data (2)), "Invalid value");

      for I in 0 .. 9 loop
         Val := Interfaces.Shift_Left (1, 7 * I);
         Encode_LEB128 (Into => Data,
                        Pos  => Data'First,
                        Val  => Val,
                        Last => Last);
         Util.Tests.Assert_Equals (T, I + 1, Integer (Last), "Invalid last position");

         Decode_LEB128 (From => Data,
                        Pos  => Data'First,
                        Val  => Res,
                        Last => Last);
         Util.Tests.Assert_Equals (T, I + 2, Integer (Last), "Invalid last position after decode");
         T.Assert (Val = Res, "Invalid decode with I " & Integer'Image (I));
      end loop;
   end Test_LEB128;

   --  ------------------------------
   --  Test encoding leb128 + base64
   --  ------------------------------
   procedure Test_Base64_LEB128 (T : in out Test) is
      use type Interfaces.Unsigned_64;

      Val   : Interfaces.Unsigned_64 := 0;
      Start : Util.Measures.Stamp;
   begin
      for I in 1 .. 100 loop
         declare
            S : constant String := Util.Encoders.Base64.Encode (Val);
            V : constant Interfaces.Unsigned_64 := Util.Encoders.Base64.Decode  (S);
         begin
            T.Assert (Val = V, "Invalid leb128+base64 encode/decode "
                      & Interfaces.Unsigned_64'Image (Val));
         end;
         Val := Val * 10 + 1;
      end loop;
      Util.Measures.Report (Start, "LEB128+Base64 encode and decode", 100);
   end Test_Base64_LEB128;

   --  ------------------------------
   --  Test HMAC-SHA256
   --  ------------------------------
   procedure Test_HMAC_SHA256_RFC4231_T1 (T : in out Test) is
      Key : constant String (1 .. 20) := (others => Character'Val (16#0b#));
   begin
      Check_HMAC256 (T, Key, "Hi There",
                     "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7");
   end Test_HMAC_SHA256_RFC4231_T1;

   procedure Test_HMAC_SHA256_RFC4231_T2 (T : in out Test) is
   begin
      Check_HMAC256 (T, "Jefe", "what do ya want for nothing?",
                     "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843");
   end Test_HMAC_SHA256_RFC4231_T2;

   procedure Test_HMAC_SHA256_RFC4231_T3 (T : in out Test) is
      Key  : constant String (1 .. 20) := (others => Character'Val (16#aa#));
      Data : constant String (1 .. 50) := (others => Character'Val (16#dd#));
   begin
      Check_HMAC256 (T, Key, Data,
                     "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe");
   end Test_HMAC_SHA256_RFC4231_T3;

   procedure Test_HMAC_SHA256_RFC4231_T4 (T : in out Test) is
      C    : constant Util.Encoders.Decoder := Create ("hex");
      Key  : constant String := Util.Encoders.Decode (C, "0102030405060708090a0b0c0d0e0f"
                                                      & "10111213141516171819");
      Data : constant String (1 .. 50) := (others => Character'Val (16#cd#));
   begin
      Check_HMAC256 (T, Key, Data,
                     "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b");
   end Test_HMAC_SHA256_RFC4231_T4;

   procedure Test_HMAC_SHA256_RFC4231_T5 (T : in out Test) is
      Key  : constant String (1 .. 20) := (others => Character'Val (16#0c#));
      H    : constant String := Util.Encoders.HMAC.SHA256.Sign (Key, "Test With Truncation");
   begin
      Assert_Equals (T, "a3b6167473100ee06e0c796c2955552b",
                     Util.Strings.Transforms.To_Lower_Case (H (1 .. 32)),
                     "Invalid HMAC-SHA256");
   end Test_HMAC_SHA256_RFC4231_T5;

   procedure Test_HMAC_SHA256_RFC4231_T6 (T : in out Test) is
      Key  : constant String (1 .. 131) := (others => Character'Val (16#aa#));
   begin
      Check_HMAC256 (T, Key, "Test Using Larger Than Block-Size Key - Hash Key First",
                     "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54");
   end Test_HMAC_SHA256_RFC4231_T6;

   procedure Test_HMAC_SHA256_RFC4231_T7 (T : in out Test) is
      Key  : constant String (1 .. 131) := (others => Character'Val (16#Aa#));
   begin
      Check_HMAC256 (T, Key, "This is a test using a larger than block-size ke"
                     & "y and a larger than block-size data. The key nee"
                     & "ds to be hashed before being used by the HMAC algorithm.",
                     "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2");
   end Test_HMAC_SHA256_RFC4231_T7;

   procedure Test_AES (T : in out Test) is
      PK   : constant Secret_Key := Create ("0123456789abcdef");
      Key  : Util.Encoders.AES.Key_Type;
      B    : Util.Encoders.AES.Block_Type := (others => 1);
      E    : Util.Encoders.AES.Block_Type := (others => 0);
      Ok   : Boolean;
   begin
      Util.Encoders.AES.Set_Encrypt_Key (Key, PK);
      Util.Encoders.AES.Encrypt (B, E, Key);
      Util.Encoders.AES.Set_Decrypt_Key (Key, PK);
      Util.Encoders.AES.Decrypt (E, B, Key);
      Ok := (for all E of B => E = 1);
      T.Assert (Ok, "Encryption and decryption are invalid (block with 1)");

      B := (others => 16#ab#);
      Util.Encoders.AES.Set_Encrypt_Key (Key, PK);
      Util.Encoders.AES.Encrypt (B, E, Key);
      Util.Encoders.AES.Set_Decrypt_Key (Key, PK);
      Util.Encoders.AES.Decrypt (E, B, Key);
      Ok := (for all E of B => E = 16#ab#);
      T.Assert (Ok, "Encryption and decryption are invalid (block with 16#AB#)");

   end Test_AES;

   --  ------------------------------
   --  Test encrypt and decrypt operations.
   --  ------------------------------
   procedure Test_Encrypt_Decrypt_Secret (T : in out Test) is
      Pk       : constant Secret_Key := Create ("0123456789abcdef");
      Cipher   : Util.Encoders.AES.Encoder;
      Decipher : Util.Encoders.AES.Decoder;
      Data     : Ada.Streams.Stream_Element_Array (1 .. 16);
      Result   : Secret_Key (16);
   begin
      Cipher.Set_Key (Pk, Util.Encoders.AES.CBC);
      Cipher.Set_Padding (Util.Encoders.AES.NO_PADDING);
      Decipher.Set_Key (Pk, Util.Encoders.AES.CBC);
      Decipher.Set_Padding (Util.Encoders.AES.NO_PADDING);
      Cipher.Encrypt_Secret (Pk, Data);
      Decipher.Decrypt_Secret (Data, Result);
      T.Assert (Result.Secret = Pk.Secret, "Encrypt_Secret and Decrypt_Secret failed");
   end Test_Encrypt_Decrypt_Secret;

   --  ------------------------------
   --  Test encrypt and decrypt operations.
   --  ------------------------------
   procedure Test_Encrypt_Decrypt_Secret_CFB (T : in out Test) is
      Pk       : constant Secret_Key := Create ("0123456789abcdef0123456789abcdef");
      Cipher   : Util.Encoders.AES.Encoder;
      Decipher : Util.Encoders.AES.Decoder;
      Data     : Ada.Streams.Stream_Element_Array (1 .. 32);
      Result   : Secret_Key (32);
   begin
      Cipher.Set_Key (Pk, Util.Encoders.AES.CFB);
      Cipher.Set_Padding (Util.Encoders.AES.NO_PADDING);
      Decipher.Set_Key (Pk, Util.Encoders.AES.CFB);
      Decipher.Set_Padding (Util.Encoders.AES.NO_PADDING);
      Cipher.Encrypt_Secret (Pk, Data);
      Decipher.Decrypt_Secret (Data, Result);
      T.Assert (Result.Secret = Pk.Secret, "CFB Encrypt_Secret and Decrypt_Secret failed");
   end Test_Encrypt_Decrypt_Secret_CFB;

   --  ------------------------------
   --  Test encrypt and decrypt operations.
   --  ------------------------------
   procedure Test_Encrypt_Decrypt_Secret_OFB (T : in out Test) is
      Pk       : constant Secret_Key := Create ("0123456789abcdef0123456789abcdef");
      Cipher   : Util.Encoders.AES.Encoder;
      Decipher : Util.Encoders.AES.Decoder;
      Data     : Ada.Streams.Stream_Element_Array (1 .. 32);
      Result   : Secret_Key (32);
   begin
      Cipher.Set_Key (Pk, Util.Encoders.AES.OFB);
      Cipher.Set_Padding (Util.Encoders.AES.NO_PADDING);
      Decipher.Set_Key (Pk, Util.Encoders.AES.OFB);
      Decipher.Set_Padding (Util.Encoders.AES.NO_PADDING);
      Cipher.Encrypt_Secret (Pk, Data);
      Decipher.Decrypt_Secret (Data, Result);
      T.Assert (Result.Secret = Pk.Secret, "OFB Encrypt_Secret and Decrypt_Secret failed");
   end Test_Encrypt_Decrypt_Secret_OFB;

   --  ------------------------------
   --  Test encrypt and decrypt operations.
   --  ------------------------------
   procedure Test_Encrypt_Decrypt_Secret_CTR (T : in out Test) is
      Pk       : constant Secret_Key := Create ("0123456789abcdef0123456789abcdef");
      Cipher   : Util.Encoders.AES.Encoder;
      Decipher : Util.Encoders.AES.Decoder;
      Data     : Ada.Streams.Stream_Element_Array (1 .. 32);
      Result   : Secret_Key (32);
   begin
      Cipher.Set_Key (Pk, Util.Encoders.AES.CTR);
      Cipher.Set_Padding (Util.Encoders.AES.NO_PADDING);
      Decipher.Set_Key (Pk, Util.Encoders.AES.CTR);
      Decipher.Set_Padding (Util.Encoders.AES.NO_PADDING);
      Cipher.Encrypt_Secret (Pk, Data);
      Decipher.Decrypt_Secret (Data, Result);
      T.Assert (Result.Secret = Pk.Secret, "CTR Encrypt_Secret and Decrypt_Secret failed");
   end Test_Encrypt_Decrypt_Secret_CTR;

   --  ------------------------------
   --  Test Decode Quoted-Printable encoding.
   --  ------------------------------
   procedure Test_Decode_Quoted_Printable (T : in out Test) is
   begin
      Assert_Equals (T, "teams aren.t =way to protect yo",
                     Quoted_Printable.Decode ("teams aren=2Et =3Dway to protect yo="));
      Assert_Equals (T, "====",
                     Quoted_Printable.Decode ("=3D=3D=3D=3D="));
      Assert_Equals (T, "teams aren.t =way to protect yo",
                     Quoted_Printable.Q_Decode ("teams_aren=2Et_=3Dway_to_protect_yo="));
   end Test_Decode_Quoted_Printable;

   --  ------------------------------
   --  Test the percent URI encoding.
   --  ------------------------------
   procedure Test_Encode_URI (T : in out Test) is
      procedure Check (Expect   : in String;
                       Item     : in String;
                       Encoding : in URI.Encoding_Array);
      procedure Check (Expect   : in String;
                       Item     : in String;
                       Encoding : in URI.Encoding_Array) is
         Len : constant Natural := URI.Encoded_Length (Item, Encoding);
         Res : constant String := URI.Encode (Item, Encoding);
      begin
         Assert_Equals (T, Len, Expect'Length, "Invalid length for " & Item);
         Assert_Equals (T, Expect, Res);
         Assert_Equals (T, Item, URI.Decode (Res));
      end Check;

   begin
      Check ("http://example.com/%5C%5B%5C", "http://example.com/\[\", URI.HREF_LOOSE);
      Check ("%20escape%20%3A%20%2F%20%23%40", " escape : / #@", URI.HREF_STRICT);
      Check ("example.com/%D1%8F%D1%87%D0%BC%D0%B5%D0%BD%D0%BD%D1%8B%D0%B9",
             "example.com/ячменный", URI.HREF_LOOSE);
   end Test_Encode_URI;

end Util.Encoders.Tests;
