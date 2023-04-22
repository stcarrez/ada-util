-----------------------------------------------------------------------
--  util-samples_tests -- Unit tests for samples
--  Copyright (C) 2023 Stephane Carrez
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

package Util.Samples_Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Encodes (T : in out Test);
   procedure Test_Cut (T : in out Test);
   procedure Test_Escape (T : in out Test);
   procedure Test_Csv_City (T : in out Test);
   procedure Test_Objcalc (T : in out Test);
   procedure Test_Log (T : in out Test);
   procedure Test_Copy (T : in out Test);
   procedure Test_Compress_Decompress (T : in out Test);
   procedure Test_Encrypt_Decrypt (T : in out Test);
   procedure Test_Lzma_Encrypt_Decrypt (T : in out Test);
   procedure Test_Lzma_Encrypt_Decrypt_B64 (T : in out Test);
   procedure Test_Multipart (T : in out Test);
   procedure Test_Dumpcert (T : in out Test);
   procedure Test_Sha256 (T : in out Test);
   procedure Test_Env (T : in out Test);
   procedure Test_Serialize (T : in out Test);
   procedure Test_Serialize_XML (T : in out Test);
   procedure Test_Proplist (T : in out Test);
   procedure Test_Properties (T : in out Test);

end Util.Samples_Tests;
