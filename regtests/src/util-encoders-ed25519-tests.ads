-----------------------------------------------------------------------
--  util-encoders-ed25519-tests - Ed25519 function tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Util.Encoders.Ed25519.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the creation of a random Ed25519 private key.
   procedure Test_Create_Private_Key (T : in out Test);

end Util.Encoders.Ed25519.Tests;
