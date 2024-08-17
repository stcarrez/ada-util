-----------------------------------------------------------------------
--  util-samples_tests -- Unit tests for samples
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
