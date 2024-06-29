-----------------------------------------------------------------------
--  util-streams-buffered-lzma-tests -- Unit tests for LZMA buffered streams
--  Copyright (C) 2018, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Util.Streams.Buffered.Lzma.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Compress_Stream (T : in out Test);

   procedure Test_Compress_File_Stream (T : in out Test);

   procedure Test_Compress_Decompress_Stream (T : in out Test);

   procedure Test_Compress_Encrypt_Decompress_Decrypt_Stream (T : in out Test);

end Util.Streams.Buffered.Lzma.Tests;
