-----------------------------------------------------------------------
--  util-encodes-kdf-tests - Key derivative function tests
--  Copyright (C) 2019, 2021 Stephane Carrez
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
with Util.Encoders.SHA1;
with Util.Encoders.SHA256;
with Util.Encoders.HMAC.SHA1;
with Util.Encoders.HMAC.SHA256;
with Util.Encoders.Base16;
with Util.Encoders.KDF.PBKDF2;
package body Util.Encoders.KDF.Tests is

   procedure PBKDF2_HMAC_SHA256 is
     new KDF.PBKDF2 (Length => Util.Encoders.SHA256.HASH_SIZE,
                     Hash   => Util.Encoders.HMAC.SHA256.Sign);

   procedure PBKDF2_HMAC_SHA1 is
     new KDF.PBKDF2 (Length => Util.Encoders.SHA1.HASH_SIZE,
                     Hash   => Util.Encoders.HMAC.SHA1.Sign);

   package Caller is new Util.Test_Caller (Test, "Encoders.KDF");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.KDF.PBKDF2.HMAC_SHA1",
                       Test_PBKDF2_HMAC_SHA1'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.KDF.PBKDF2.HMAC_SHA256",
                       Test_PBKDF2_HMAC_SHA256'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test derived key generation with HMAC-SHA1.
   --  ------------------------------
   procedure Test_PBKDF2_HMAC_SHA1 (T : in out Test) is
      Pass : Secret_Key := Create (Password => "password");
      Salt : Secret_Key := Create (Password => "salt");
      Key  : Secret_Key (Length => 20);
      Hex  : Util.Encoders.Base16.Encoder;
   begin
      --  RFC6070 - test vector 1
      PBKDF2_HMAC_SHA1 (Pass, Salt, 1, Key);

      Util.Tests.Assert_Equals (T, "0C60C80F961F0E71F3A9B524AF6012062FE037A6",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA1 test vector 1");

      --  RFC6070 - test vector 2
      PBKDF2_HMAC_SHA1 (Pass, Salt, 2, Key);

      Util.Tests.Assert_Equals (T, "EA6C014DC72D6F8CCD1ED92ACE1D41F0D8DE8957",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA1 test vector 2");

      --  RFC6070 - test vector 3
      PBKDF2_HMAC_SHA1 (Pass, Salt, 4096, Key);

      Util.Tests.Assert_Equals (T, "4B007901B765489ABEAD49D926F721D065A429C1",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA1 test vector 3");
   end Test_PBKDF2_HMAC_SHA1;

   --  ------------------------------
   --  Test derived key generation with HMAC-SHA1.
   --  ------------------------------
   procedure Test_PBKDF2_HMAC_SHA256 (T : in out Test) is
      Pass : Secret_Key := Create (Password => "password");
      Salt : Secret_Key := Create (Password => "salt");
      Key  : Secret_Key (Length => 32);
      Hex  : Util.Encoders.Base16.Encoder;
   begin
      --  RFC6070 - test vector 1
      PBKDF2_HMAC_SHA256 (Pass, Salt, 1, Key);

      Util.Tests.Assert_Equals (T, "120FB6CFFCF8B32C43E7225256C4F837A8"
                                & "6548C92CCC35480805987CB70BE17B",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA256 test vector 1");

      --  RFC6070 - test vector 2
      PBKDF2_HMAC_SHA256 (Pass, Salt, 2, Key);

      Util.Tests.Assert_Equals (T, "AE4D0C95AF6B46D32D0ADFF928F06DD02A"
                                & "303F8EF3C251DFD6E2D85A95474C43",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA256 test vector 2");

      --  RFC6070 - test vector 3
      PBKDF2_HMAC_SHA256 (Pass, Salt, 4096, Key);

      Util.Tests.Assert_Equals (T, "C5E478D59288C841AA530DB6845C4C8D96"
                                & "2893A001CE4E11A4963873AA98134A",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA256 test vector 3");
   end Test_PBKDF2_HMAC_SHA256;

end Util.Encoders.KDF.Tests;
