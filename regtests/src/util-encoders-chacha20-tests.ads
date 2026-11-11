-----------------------------------------------------------------------
--  util-encoders-chacha20-tests - ChaCha20 cipher tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Util.Encoders.ChaCha20.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the ChaCha20 block function with the RFC 8439 test vector.
   procedure Test_Block_Vector (T : in out Test);

   --  Test ChaCha20 encryption with the RFC 8439 test vector.
   procedure Test_Encrypt_Vector (T : in out Test);

   --  Test ChaCha20 encryption with several Transform calls on small buffers.
   procedure Test_Encrypt_Chunked (T : in out Test);

   --  Test ChaCha20 decryption with the RFC 8439 test vector.
   procedure Test_Decrypt_Vector (T : in out Test);

   --  Test ChaCha20 encryption/decryption through the Util.Streams.ChaCha20 streams.
   procedure Test_Streams (T : in out Test);

end Util.Encoders.ChaCha20.Tests;
