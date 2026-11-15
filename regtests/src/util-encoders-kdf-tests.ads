-----------------------------------------------------------------------
--  util-encodes-kdf-tests - Key derivative function tests
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Util.Encoders.KDF.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test derived key generation with HMAC-SHA1.
   procedure Test_PBKDF2_HMAC_SHA1 (T : in out Test);

   --  Test derived key generation with HMAC-SHA256.
   procedure Test_PBKDF2_HMAC_SHA256 (T : in out Test);

   --  Test HKDF (RFC 5869) extract+expand with HMAC-SHA256.
   procedure Test_HKDF_SHA256_RFC5869_A1 (T : in out Test);
   procedure Test_HKDF_SHA256_RFC5869_A2 (T : in out Test);
   procedure Test_HKDF_SHA256_RFC5869_A3 (T : in out Test);

   --  Test HKDF extract+expand with HMAC-SHA512.
   procedure Test_HKDF_SHA512 (T : in out Test);

   --  Test HKDF Derive_Key with HMAC-SHA512.
   procedure Test_HKDF_SHA512_Derive_Key (T : in out Test);

end Util.Encoders.KDF.Tests;
