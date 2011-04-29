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

with Util.Test_Caller;
with Util.Tests;
with Util.Measures;
with Ada.Text_IO;
with Util.Encoders.SHA1;
--  with Util.Log.Loggers;
package body Util.Encoders.Tests is

   use Util.Tests;
--     use Util.Log;
--
--     Log : constant Loggers.Logger := Loggers.Create ("Util.Encoders.Tests");

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.Base16.Encode",
                       Test_Hex'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base16.Decode",
                       Test_Hex'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base64.Encode",
                       Test_Base64_Encode'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base64.Decode",
                       Test_Base64_Decode'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.Base64.Benchmark",
                       Test_Base64_Benchmark'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.SHA1.Encode",
                       Test_SHA1_Encode'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.SHA1.Benchmark",
                       Test_SHA1_Benchmark'Access);
   end Add_Tests;

   procedure Test_Base64_Encode (T : in out Test) is
      C : constant Util.Encoders.Encoder := Create ("base64");
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
      C   : Util.Encoders.SHA1.Context;
      E   : Util.Encoders.Encoder := Create ("sha1");
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

end Util.Encoders.Tests;
