-----------------------------------------------------------------------
--  util-encoders-x25519-tests - X25519 function tests
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Test_Caller;
with Util.Encoders.AEAD;
with Util.Encoders.KDF.HKDF_SHA512;
with Util.Encoders.Random.Pseudo;
with Util.Encoders.Random.System;

package body Util.Encoders.X25519.Tests is

   package Caller is new Util.Test_Caller (Test, "Encoders.X25519");

   Hex_Dec : constant Util.Encoders.Decoder := Util.Encoders.Create ("hex");
   Hex_Enc : constant Util.Encoders.Encoder := Util.Encoders.Create ("hex");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.X25519.Create_Private_Key",
                       Test_Create_Private_Key'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.X25519 (RFC 7748 vector)",
                       Test_RFC7748_Vector'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.X25519 (AEAD roundtrip)",
                       Test_AEAD_Roundtrip'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the creation of a random X25519 private key and the Diffie-Hellman
   --  shared secret agreement.
   --  ------------------------------
   procedure Test_Create_Private_Key (T : in out Test) is
      Gen  : Util.Encoders.Random.System.System_Generator;
      Pse  : Util.Encoders.Random.Pseudo.Pseudo_Generator;
      K1   : Private_Key;
      K2   : Private_Key;
      K3   : Private_Key;
      Pub1 : Public_Key;
      Pub2 : Public_Key;
   begin
      Create_Private_Key (Gen, K1);
      Create_Private_Key (Gen, K2);
      Create_Private_Key (Pse, K3);

      Pub1 := Get_Public_Key (K1);
      Pub2 := Get_Public_Key (K2);
      T.Assert (Pub1 /= Pub2, "Two created private keys have the same public key");
      T.Assert (Pub1 /= Get_Public_Key (K3), "Created keys are not independent");

      --  Diffie-Hellman: both sides must compute the same shared secret.
      declare
         S1, S2 : Shared_Key;
      begin
         Shared_Secret (K1, Pub2, S1);
         Shared_Secret (K2, Pub1, S2);
         T.Assert (S1.Secret = S2.Secret,
                   "X25519 shared secrets differ");
      end;
   end Test_Create_Private_Key;

   --  ------------------------------
   --  Test the X25519 function with the RFC 7748 section 6.1 test vector.
   --  ------------------------------
   procedure Test_RFC7748_Vector (T : in out Test) is
      Alice_Priv   : constant String :=
        "77076D0A7318A57D3C16C17251B26645DF4C2F87EBC0992AB177FBA51DB92C2A";
      Alice_Pub    : constant String :=
        "8520F0098930A754748B7DDCB43EF75A0DBF3A0D26381AF4EBA4A98EAA9B4E6A";
      Bob_Priv     : constant String :=
        "5DAB087E624A8A4B79E17F8B83800EE66F3BB1292618B6FD1C2F8B27FF88E0EB";
      Bob_Pub      : constant String :=
        "DE9EDB7D7B7DC1B4D35B61C2ECE435373F8343C85B78674DADFC7E146F882B4F";
      Shared_Hex   : constant String :=
        "4A5D9D5BA4CE2DE1728E3BF480350F25E07E21C947D19E3376F09B3C1E161742";
      Alice : Private_Key;
      Bob   : Private_Key;
   begin
      Alice.Secret := Hex_Dec.Decode_Binary (Alice_Priv);
      Bob.Secret   := Hex_Dec.Decode_Binary (Bob_Priv);
      Util.Tests.Assert_Equals (T, Alice_Pub, Hex_Enc.Encode_Binary (Get_Public_Key (Alice)),
                                "Invalid Alice X25519 public key");
      Util.Tests.Assert_Equals (T, Bob_Pub, Hex_Enc.Encode_Binary (Get_Public_Key (Bob)),
                                "Invalid Bob X25519 public key");

      declare
         Secret : Shared_Key;
      begin
         Shared_Secret (Alice, Get_Public_Key (Bob), Secret);
         Util.Tests.Assert_Equals
           (T, Shared_Hex,
            Hex_Enc.Encode_Binary (Secret.Secret),
            "Invalid X25519 shared secret");

         Shared_Secret (Bob, Get_Public_Key (Alice), Secret);
         Util.Tests.Assert_Equals
           (T, Shared_Hex,
            Hex_Enc.Encode_Binary (Secret.Secret),
            "Invalid X25519 shared secret (other direction)");
      end;
   end Test_RFC7748_Vector;

   --  ------------------------------
   --  Test an X25519 key exchange followed by an AEAD encryption/decryption
   --  round trip using the Util.Encoders.AEAD package.
   --  ------------------------------
   procedure Test_AEAD_Roundtrip (T : in out Test) is
      Msg     : constant Stream_Element_Array
        := Hex_Dec.Decode_Binary ("48656c6c6f");  --  "Hello"
      Nonce   : constant Util.Encoders.AEAD.Nonce
        := Util.Encoders.AEAD.Nonce (Hex_Dec.Decode_Binary ("000000000000004A00000000"));
      AAD     : constant Stream_Element_Array := Hex_Dec.Decode_Binary ("01020304");
      Gen     : Util.Encoders.Random.System.System_Generator;
      K1      : Private_Key;
      K2      : Private_Key;
      Pub1    : Public_Key;
      Pub2    : Public_Key;
      Shared  : Shared_Key;
      Key     : Util.Encoders.Secret_Key (Length => Util.Encoders.AEAD.Key_Size);
      Info    : Stream_Element_Array (1 .. 64);
      Salt    : constant Stream_Element_Array (1 .. 64) := [others => 0];
      Cipher  : Stream_Element_Array (1 .. Msg'Length);
      Tag     : Util.Encoders.AEAD.Tag;
      Data    : Stream_Element_Array (1 .. Msg'Length + Util.Encoders.AEAD.Tag_Size);
      Plain   : Stream_Element_Array (1 .. Msg'Length);
      Last    : Stream_Element_Offset;
      Encoded : Stream_Element_Offset;
      Secret  : Shared_Key;
   begin
      Create_Private_Key (Gen, K1);
      Create_Private_Key (Gen, K2);
      Pub1 := Get_Public_Key (K1);
      Pub2 := Get_Public_Key (K2);
      Shared_Secret (K1, Pub2, Secret);
      Shared_Secret (K2, Pub1, Shared);
      T.Assert (Shared.Secret = Secret.Secret,
                "X25519 shared secrets differ");

      --  Derive the AEAD key from the shared secret with HKDF-SHA512, binding
      --  both public keys in the info.
      Info (1 .. 32)  := Pub1;
      Info (33 .. 64) := Pub2;
      Util.Encoders.KDF.HKDF_SHA512.Derive_Key (Salt   => Salt,
                                                IKM    => Shared.Secret,
                                                Info   => Info,
                                                Result => Key);

      --  Encrypt the message with Util.Encoders.AEAD.
      declare
         E : Util.Encoders.AEAD.Encoder;
         L : Stream_Element_Offset := 0;
      begin
         E.Set_Key (Key);
         E.Set_Nonce (Nonce);
         E.Update_AAD (AAD);
         E.Transform (Msg, Cipher, Last, Encoded);
         T.Assert (Last = Cipher'Last and then Encoded = Msg'Last,
                   "Invalid AEAD encoder Transform positions");
         E.Finish (Tag, L);
         T.Assert (L = Tag'Last, "Invalid AEAD encoder Finish position");
      end;
      Data (1 .. Msg'Length)           := Cipher;
      Data (Msg'Length + 1 .. Data'Last) := Tag;

      --  Decrypt and verify the message with Util.Encoders.AEAD.
      declare
         D : Util.Encoders.AEAD.Decoder;
         L : Stream_Element_Offset := 0;
      begin
         D.Set_Key (Key);
         D.Set_Nonce (Nonce);
         D.Update_AAD (AAD);
         D.Transform (Data, Plain, Last, Encoded);
         T.Assert (Last = Plain'Last and then Encoded = Data'Last,
                   "Invalid AEAD decoder Transform positions");
         D.Finish (Tag, L);
      end;
      T.Assert (Plain = Msg, "AEAD decrypted message differs from the original");

      --  A tampered authentication tag must be rejected.
      declare
         Bad : Stream_Element_Array := Data;
      begin
         Bad (Bad'Last) := Bad (Bad'Last) xor 1;
         declare
            D : Util.Encoders.AEAD.Decoder;
            L : Stream_Element_Offset := 0;
         begin
            D.Set_Key (Key);
            D.Set_Nonce (Nonce);
            D.Update_AAD (AAD);
            D.Transform (Bad, Plain, Last, Encoded);
            D.Finish (Tag, L);
            T.Fail ("Encoding_Error not raised for a tampered AEAD tag");
         exception
            when Util.Encoders.Encoding_Error =>
               null;
         end;
      end;

      --  A wrong AAD must be rejected.
      declare
         D : Util.Encoders.AEAD.Decoder;
         L : Stream_Element_Offset := 0;
      begin
         D.Set_Key (Key);
         D.Set_Nonce (Nonce);
         D.Update_AAD (Hex_Dec.Decode_Binary ("0BAD"));
         D.Transform (Data, Plain, Last, Encoded);
         D.Finish (Tag, L);
         T.Fail ("Encoding_Error not raised for a wrong AAD");
      exception
         when Util.Encoders.Encoding_Error =>
            null;
      end;
   end Test_AEAD_Roundtrip;

end Util.Encoders.X25519.Tests;
