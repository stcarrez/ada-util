-----------------------------------------------------------------------
--  util-encoders-x25519-tests - X25519 function tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Tests;
package Util.Encoders.X25519.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the creation of a random X25519 private key and the Diffie-Hellman
   --  shared secret agreement.
   procedure Test_Create_Private_Key (T : in out Test);

   --  Test the X25519 function with the RFC 7748 section 6.1 test vector.
   procedure Test_RFC7748_Vector (T : in out Test);

   --  Test an X25519 key exchange followed by an AEAD encryption/decryption
   --  round trip using the Util.Encoders.AEAD package.
   procedure Test_AEAD_Roundtrip (T : in out Test);

end Util.Encoders.X25519.Tests;
