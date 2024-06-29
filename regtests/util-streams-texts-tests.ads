-----------------------------------------------------------------------
--  streams.texts.tests -- Unit tests for text streams
--  Copyright (C) 2012, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Streams.Texts.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test reading a text stream.
   procedure Test_Read_Line (T : in out Test);

   --  Write on a text stream converting an integer and writing it.
   procedure Test_Write_Integer (T : in out Test);

   --  Write on a text stream converting an integer and writing it.
   procedure Test_Write_Long_Integer (T : in out Test);

   --  Write on a text stream.
   procedure Test_Write (T : in out Test);

end Util.Streams.Texts.Tests;
