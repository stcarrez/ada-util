-----------------------------------------------------------------------
--  util-encodes-ecc-tests - ECC function tests
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Util.Encoders.ECC.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test ECC correction on 256 bytes block.
   procedure Test_ECC_Block_256 (T : in out Test);

   --  Test ECC correction on 512 bytes block.
   procedure Test_ECC_Block_512 (T : in out Test);

end Util.Encoders.ECC.Tests;
