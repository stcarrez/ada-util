-----------------------------------------------------------------------
--  streams.buffered.tests -- Unit tests for buffered streams
--  Copyright (C) 2010, 2011, 2020 Stephane Carrez
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

end Util.Streams.Buffered.Tests;
