-----------------------------------------------------------------------
--  util-encodes-tests - Test for encoding
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with AUnit.Test_Caller;
with AUnit.Assertions;
with Util.Files;
with Util.Tests;
with Util.Measures;
with Ada.Text_IO;
package body Util.Encoders.Tests is

   use Util.Tests;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Suite.Add_Test (Caller.Create ("Test Util.Encoders.Base16.Encode",
        Test_Hex'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Encoders.Base16.Decode",
        Test_Hex'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Encoders.Base64.Encode",
        Test_Base64_Encode'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Encoders.Base64.Decode",
        Test_Base64_Decode'Access));
      Suite.Add_Test (Caller.Create ("Test Util.Encoders.Base64.Benchmark",
        Test_Base64_Benchmark'Access));
   end Add_Tests;

   procedure Test_Base64_Encode (T : in out Test) is
      C : Util.Encoders.Encoder := Create ("base64");
   begin
      Assert_Equals (T, "YQ==", Util.Encoders.Encode (C, "a"));
      Assert_Equals (T, "fA==", Util.Encoders.Encode (C, "|"));
      Assert_Equals (T, "fHw=", Util.Encoders.Encode (C, "||"));
      Assert_Equals (T, "fH5+", Util.Encoders.Encode (C, "|~~"));
   end Test_Base64_Encode;

   procedure Test_Base64_Decode (T : in out Test) is
      C : Util.Encoders.Encoder := Create ("base64");
   begin
      Assert_Equals (T, "a", Util.Encoders.Decode (C, "YQ=="));
      Assert_Equals (T, "|", Util.Encoders.Decode (C, "fA=="));
      Assert_Equals (T, "||", Util.Encoders.Decode (C, "fHw="));
      Assert_Equals (T, "|~~", Util.Encoders.Decode (C, "fH5+"));
      Test_Encoder (T, C);
   end Test_Base64_Decode;

   procedure Test_Encoder (T : in out Test;
                           C : in out Util.Encoders.Encoder) is
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
               D : constant String := Util.Encoders.Decode (C, E);
            begin
               Assert_Equals (T, Pattern, D, "Encoding failed for length "
                              & Integer'Image (I));
            end;
         exception
            when others =>
               Ada.Text_IO.Put_Line ("Error at index " & Integer'Image (I));
               raise;
         end;
      end loop;
   end Test_Encoder;

   procedure Test_Hex (T : in out Test) is
      C : Util.Encoders.Encoder := Create ("hex");
   begin
      Assert_Equals (T, "41424344", Util.Encoders.Encode(C, "ABCD"));
      Assert_Equals (T, "ABCD", Util.Encoders.Decode (C, "41424344"));
      Test_Encoder (T, C);
   end Test_Hex;

   procedure Test_Base64_Benchmark (T : in out Test) is
      C : Util.Encoders.Encoder := Create ("base64");
      S : String (1 .. 1_024) := (others => 'a');
   begin
      declare
         T : Util.Measures.Stamp;
         R : constant String := Util.Encoders.Encode (C, S);
      begin
         Util.Measures.Report (T, "Base64 encode 1024 bytes");
      end;
   end Test_Base64_Benchmark;

end Util.Encoders.Tests;
