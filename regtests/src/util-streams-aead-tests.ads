-----------------------------------------------------------------------
--  util-streams-aead-tests - AEAD (ChaCha20-Poly1305) stream tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Tests;
package Util.Streams.AEAD.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test AEAD encryption/decryption through the Util.Streams.AEAD streams.
   procedure Test_Streams (T : in out Test);

   --  Test AEAD encryption through the stream with the RFC 8439 section 2.8.2 vector.
   procedure Test_Encrypt_Vector (T : in out Test);

   --  Test AEAD decryption through the stream with the RFC 8439 section 2.8.2 vector.
   procedure Test_Decrypt_Vector (T : in out Test);

   --  Test AEAD encryption to a file and decryption from the file.
   procedure Test_File (T : in out Test);

   --  Test that a corrupted authentication tag is detected when reading the stream.
   procedure Test_Tampered_Tag (T : in out Test);

   --  Test that a wrong AAD is detected when reading the stream.
   procedure Test_Wrong_AAD (T : in out Test);

end Util.Streams.AEAD.Tests;
