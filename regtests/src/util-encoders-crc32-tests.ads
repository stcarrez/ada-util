-----------------------------------------------------------------------
--  util-encoders-crc32-tests - CRC32 function tests
--  Copyright (C) 2025-2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Util.Encoders.CRC32.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the standard CRC-32 test vectors (CRC-32/ISO-HDLC).
   procedure Test_CRC32_Vectors (T : in out Test);

   --  Test crc32 on string
   procedure Test_CRC32_String (T : in out Test);

   --  Test crc32 on binary
   procedure Test_CRC32_Binary (T : in out Test);

   --  Test crc32 computed with several Update calls.
   procedure Test_CRC32_Incremental (T : in out Test);

end Util.Encoders.CRC32.Tests;
