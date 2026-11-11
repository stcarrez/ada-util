-----------------------------------------------------------------------
--  util-encoders-poly1305-tests - Poly1305 authenticator tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Util.Encoders.Poly1305.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the Poly1305 one-shot Mac with the RFC 8439 test vector.
   procedure Test_Mac_Vector (T : in out Test);

   --  Test the Poly1305 incremental Context (Set_Key/Update/Finish) with
   --  several Update calls.
   procedure Test_Mac_Incremental (T : in out Test);

end Util.Encoders.Poly1305.Tests;
