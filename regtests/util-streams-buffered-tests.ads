-----------------------------------------------------------------------
--  streams.buffered.tests -- Unit tests for buffered streams
--  Copyright (C) 2010, 2011, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Streams.Buffered.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Read_Write (T : in out Test);
   procedure Test_Write (T : in out Test);
--     procedure Test_Read_File_Missing (T : in out Test);
--     procedure Test_Read_File_Truncate (T : in out Test);
--     procedure Test_Write_File (T : in out Test);
--     procedure Test_Find_File_Path (T : in out Test);
--  Write on a buffer and force regular flush on a larger buffer
   procedure Test_Write_Stream (T : in out Test);

   --  Test reading UTF-8 file.
   procedure Test_Read_UTF_8 (T : in out Test);

   --  Test reading a streams with several parts separated by boundaries.
   procedure Test_Parts (T : in out Test);
   procedure Test_Parts_2 (T : in out Test);
   procedure Test_Parts_3 (T : in out Test);
   procedure Test_Parts_4 (T : in out Test);
   procedure Test_Parts_5 (T : in out Test);

end Util.Streams.Buffered.Tests;
