-----------------------------------------------------------------------
--  streams.files.tests -- Unit tests for buffered streams
--  Copyright (C) 2010, 2011, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Streams.Files.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test reading and writing on a buffered stream with various buffer sizes
   procedure Test_Read_Write (T : in out Test);
   procedure Test_Write (T : in out Test);
   procedure Test_Copy_Stream (T : in out Test);

end Util.Streams.Files.Tests;
