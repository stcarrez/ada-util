-----------------------------------------------------------------------
--  util-log-lzma-tests - Test for rolling file with LZMA compression
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Log.Lzma_Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the rolling file appender.
   procedure Test_Rolling_File_Appender (T : in out Test);

end Util.Log.Lzma_Tests;
