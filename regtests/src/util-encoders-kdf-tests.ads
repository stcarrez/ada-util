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

end Util.Encoders.KDF.Tests;
