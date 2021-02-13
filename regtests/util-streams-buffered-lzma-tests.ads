-----------------------------------------------------------------------
--  util-streams-buffered-lzma-tests -- Unit tests for LZMA buffered streams
--  Copyright (C) 2018, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
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
