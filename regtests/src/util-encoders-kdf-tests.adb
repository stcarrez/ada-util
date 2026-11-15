-----------------------------------------------------------------------
--  util-encodes-kdf-tests - Key derivative function tests
--  Copyright (C) 2019, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Encoders.SHA1;
with Util.Encoders.SHA256;
with Util.Encoders.HMAC.SHA1;
with Util.Encoders.HMAC.SHA256;
with Util.Encoders.Base16;
with Util.Encoders.KDF.PBKDF2;
with Util.Encoders.KDF.HKDF_SHA256;
with Util.Encoders.KDF.HKDF_SHA512;
with Util.Encoders.HMAC.SHA512;
package body Util.Encoders.KDF.Tests is

   procedure PBKDF2_HMAC_SHA256 is
     new KDF.PBKDF2 (Length => Util.Encoders.SHA256.HASH_SIZE,
                     Hash   => Util.Encoders.HMAC.SHA256.Sign);

   procedure PBKDF2_HMAC_SHA1 is
     new KDF.PBKDF2 (Length => Util.Encoders.SHA1.HASH_SIZE,
                     Hash   => Util.Encoders.HMAC.SHA1.Sign);

   package Caller is new Util.Test_Caller (Test, "Encoders.KDF");

   procedure Check_HKDF256 (T           : in out Test'Class;
                            Salt, IKM, Info : in String;
                            Expect_PRK  : in String;
                            Expect_OKM  : in String;
                            No_Salt     : in Boolean := False);
   procedure Check_HKDF512 (T           : in out Test'Class;
                            Salt, IKM, Info : in String;
                            Expect_PRK  : in String;
                            Expect_OKM  : in String;
                            No_Salt     : in Boolean := False);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.KDF.PBKDF2.HMAC_SHA1",
                       Test_PBKDF2_HMAC_SHA1'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.KDF.PBKDF2.HMAC_SHA256",
                       Test_PBKDF2_HMAC_SHA256'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.KDF.HKDF_SHA256 (RFC5869 A.1)",
                       Test_HKDF_SHA256_RFC5869_A1'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.KDF.HKDF_SHA256 (RFC5869 A.2)",
                       Test_HKDF_SHA256_RFC5869_A2'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.KDF.HKDF_SHA256 (RFC5869 A.3)",
                       Test_HKDF_SHA256_RFC5869_A3'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.KDF.HKDF_SHA512",
                       Test_HKDF_SHA512'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.KDF.HKDF_SHA512.Derive_Key",
                       Test_HKDF_SHA512_Derive_Key'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test derived key generation with HMAC-SHA1.
   --  ------------------------------
   procedure Test_PBKDF2_HMAC_SHA1 (T : in out Test) is
      Pass : Secret_Key := Create (Password => "password");
      Salt : Secret_Key := Create (Password => "salt");
      Key  : Secret_Key (Length => 20);
      Hex  : Util.Encoders.Base16.Encoder;
   begin
      --  RFC6070 - test vector 1
      PBKDF2_HMAC_SHA1 (Pass, Salt, 1, Key);

      Util.Tests.Assert_Equals (T, "0C60C80F961F0E71F3A9B524AF6012062FE037A6",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA1 test vector 1");

      --  RFC6070 - test vector 2
      PBKDF2_HMAC_SHA1 (Pass, Salt, 2, Key);

      Util.Tests.Assert_Equals (T, "EA6C014DC72D6F8CCD1ED92ACE1D41F0D8DE8957",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA1 test vector 2");

      --  RFC6070 - test vector 3
      PBKDF2_HMAC_SHA1 (Pass, Salt, 4096, Key);

      Util.Tests.Assert_Equals (T, "4B007901B765489ABEAD49D926F721D065A429C1",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA1 test vector 3");
   end Test_PBKDF2_HMAC_SHA1;

   --  ------------------------------
   --  Test derived key generation with HMAC-SHA1.
   --  ------------------------------
   procedure Test_PBKDF2_HMAC_SHA256 (T : in out Test) is
      Pass : Secret_Key := Create (Password => "password");
      Salt : Secret_Key := Create (Password => "salt");
      Key  : Secret_Key (Length => 32);
      Hex  : Util.Encoders.Base16.Encoder;
   begin
      --  RFC6070 - test vector 1
      PBKDF2_HMAC_SHA256 (Pass, Salt, 1, Key);

      Util.Tests.Assert_Equals (T, "120FB6CFFCF8B32C43E7225256C4F837A8"
                                & "6548C92CCC35480805987CB70BE17B",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA256 test vector 1");

      --  RFC6070 - test vector 2
      PBKDF2_HMAC_SHA256 (Pass, Salt, 2, Key);

      Util.Tests.Assert_Equals (T, "AE4D0C95AF6B46D32D0ADFF928F06DD02A"
                                & "303F8EF3C251DFD6E2D85A95474C43",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA256 test vector 2");

      --  RFC6070 - test vector 3
      PBKDF2_HMAC_SHA256 (Pass, Salt, 4096, Key);

      Util.Tests.Assert_Equals (T, "C5E478D59288C841AA530DB6845C4C8D96"
                                & "2893A001CE4E11A4963873AA98134A",
                                Hex.Transform (Key.Secret),
                                "PBKDF2-HMAC-SHA256 test vector 3");
   end Test_PBKDF2_HMAC_SHA256;

   --  ------------------------------
   --  Check the HKDF-SHA256 extract and expand with the given test vector.
   --  ------------------------------
   procedure Check_HKDF256 (T           : in out Test'Class;
                            Salt, IKM, Info : in String;
                            Expect_PRK  : in String;
                            Expect_OKM  : in String;
                            No_Salt     : in Boolean := False) is
      Dec    : constant Util.Encoders.Decoder := Util.Encoders.Create ("hex");
      Enc    : constant Util.Encoders.Encoder := Util.Encoders.Create ("hex");
      IKM_B  : constant Stream_Element_Array := Dec.Decode_Binary (IKM);
      Info_B : constant Stream_Element_Array := Dec.Decode_Binary (Info);
      Salt_B : constant Stream_Element_Array := Dec.Decode_Binary (Salt);
      PRK    : Stream_Element_Array (1 .. Util.Encoders.SHA256.HASH_SIZE);
      OKM    : Stream_Element_Array (1 .. Expect_OKM'Length / 2);
   begin
      if No_Salt then
         Util.Encoders.KDF.HKDF_SHA256.Extract (IKM_B, PRK);
      else
         Util.Encoders.KDF.HKDF_SHA256.Extract (Salt_B, IKM_B, PRK);
      end if;
      Util.Tests.Assert_Equals (T, Expect_PRK, Enc.Encode_Binary (PRK),
                                "Invalid HKDF-SHA256 PRK");
      Util.Encoders.KDF.HKDF_SHA256.Expand (PRK, Info_B, OKM);
      Util.Tests.Assert_Equals (T, Expect_OKM, Enc.Encode_Binary (OKM),
                                "Invalid HKDF-SHA256 OKM");
   end Check_HKDF256;

   --  ------------------------------
   --  Check the HKDF-SHA512 extract and expand with the given test vector.
   --  ------------------------------
   procedure Check_HKDF512 (T           : in out Test'Class;
                            Salt, IKM, Info : in String;
                            Expect_PRK  : in String;
                            Expect_OKM  : in String;
                            No_Salt     : in Boolean := False) is
      Dec    : constant Util.Encoders.Decoder := Util.Encoders.Create ("hex");
      Enc    : constant Util.Encoders.Encoder := Util.Encoders.Create ("hex");
      IKM_B  : constant Stream_Element_Array := Dec.Decode_Binary (IKM);
      Info_B : constant Stream_Element_Array := Dec.Decode_Binary (Info);
      Salt_B : constant Stream_Element_Array := Dec.Decode_Binary (Salt);
      PRK    : Stream_Element_Array (1 .. Util.Encoders.HMAC.SHA512.HASH_SIZE);
      OKM    : Stream_Element_Array (1 .. Expect_OKM'Length / 2);
   begin
      if No_Salt then
         Util.Encoders.KDF.HKDF_SHA512.Extract (IKM_B, PRK);
      else
         Util.Encoders.KDF.HKDF_SHA512.Extract (Salt_B, IKM_B, PRK);
      end if;
      Util.Tests.Assert_Equals (T, Expect_PRK, Enc.Encode_Binary (PRK),
                                "Invalid HKDF-SHA512 PRK");
      Util.Encoders.KDF.HKDF_SHA512.Expand (PRK, Info_B, OKM);
      Util.Tests.Assert_Equals (T, Expect_OKM, Enc.Encode_Binary (OKM),
                                "Invalid HKDF-SHA512 OKM");
   end Check_HKDF512;

   --  ------------------------------
   --  Test HKDF-SHA256 with the RFC 5869 appendix A.1 test case.
   --  ------------------------------
   procedure Test_HKDF_SHA256_RFC5869_A1 (T : in out Test) is
   begin
      Check_HKDF256 (T,
                     Salt => "000102030405060708090A0B0C",
                     IKM  => "0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B",
                     Info => "F0F1F2F3F4F5F6F7F8F9",
                     Expect_PRK =>
                       "077709362C2E32DF0DDC3F0DC47BBA63"
                       & "90B6C73BB50F9C3122EC844AD7C2B3E5",
                     Expect_OKM =>
                       "3CB25F25FAACD57A90434F64D0362F2A"
                       & "2D2D0A90CF1A5A4C5DB02D56ECC4C5BF"
                       & "34007208D5B887185865");
   end Test_HKDF_SHA256_RFC5869_A1;

   --  ------------------------------
   --  Test HKDF-SHA256 with the RFC 5869 appendix A.2 test case (long inputs).
   --  ------------------------------
   procedure Test_HKDF_SHA256_RFC5869_A2 (T : in out Test) is
   begin
      Check_HKDF256 (T,
                     Salt =>
                       "606162636465666768696A6B6C6D6E6F"
                       & "707172737475767778797A7B7C7D7E7F"
                       & "808182838485868788898A8B8C8D8E8F"
                       & "909192939495969798999A9B9C9D9E9F"
                       & "A0A1A2A3A4A5A6A7A8A9AAABACADAEAF",
                     IKM  =>
                       "000102030405060708090A0B0C0D0E0F"
                       & "101112131415161718191A1B1C1D1E1F"
                       & "202122232425262728292A2B2C2D2E2F"
                       & "303132333435363738393A3B3C3D3E3F"
                       & "404142434445464748494A4B4C4D4E4F",
                     Info =>
                       "B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF"
                       & "C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF"
                       & "D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF"
                       & "E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF"
                       & "F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF",
                     Expect_PRK =>
                       "06A6B88C5853361A06104C9CEB35B45C"
                       & "EF760014904671014A193F40C15FC244",
                     Expect_OKM =>
                       "B11E398DC80327A1C8E7F78C596A4934"
                       & "4F012EDA2D4EFAD8A050CC4C19AFA97C"
                       & "59045A99CAC7827271CB41C65E590E09"
                       & "DA3275600C2F09B8367793A9ACA3DB7"
                       & "1CC30C58179EC3E87C14C01D5C1F3434"
                       & "F1D87");
   end Test_HKDF_SHA256_RFC5869_A2;

   --  ------------------------------
   --  Test HKDF-SHA256 with the RFC 5869 appendix A.3 test case (no salt, no info).
   --  ------------------------------
   procedure Test_HKDF_SHA256_RFC5869_A3 (T : in out Test) is
   begin
      Check_HKDF256 (T,
                     Salt => "",
                     IKM  => "0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B",
                     Info => "",
                     No_Salt => True,
                     Expect_PRK =>
                       "19EF24A32C717B167F33A91D6F648BDF"
                       & "96596776AFDB6377AC434C1C293CCB04",
                     Expect_OKM =>
                       "8DA4E775A563C18F715F802A063C5A31"
                       & "B8A11F5C5EE1879EC3454E5F3C738D2D"
                       & "9D201395FAA4B61A96C8");
   end Test_HKDF_SHA256_RFC5869_A3;

   --  ------------------------------
   --  Test HKDF-SHA512 extract and expand (single block, multi block, no salt).
   --  ------------------------------
   procedure Test_HKDF_SHA512 (T : in out Test) is
   begin
      --  Same inputs as RFC 5869 A.1, L = 42 (single block).
      Check_HKDF512 (T,
                     Salt => "000102030405060708090A0B0C",
                     IKM  => "0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B",
                     Info => "F0F1F2F3F4F5F6F7F8F9",
                     Expect_PRK =>
                       "665799823737DED04A88E47E54A5890B"
                       & "B2C3D247C7A4254A8E61350723590A26"
                       & "C36238127D8661B88CF80EF802D57E2F"
                       & "7CEBCF1E00E083848BE19929C61B4237",
                     Expect_OKM =>
                       "832390086CDA71FB47625BB5CEB168E4"
                       & "C8E26A1A16ED34D9FC7FE92C14815793"
                       & "38DA362CB8D9F925D7CB");

      --  L = 100: two expansion blocks.
      Check_HKDF512 (T,
                     Salt => "000102030405060708090A0B0C",
                     IKM  => "0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B",
                     Info => "696E666F2D64617461",
                     Expect_PRK =>
                       "665799823737DED04A88E47E54A5890B"
                       & "B2C3D247C7A4254A8E61350723590A26"
                       & "C36238127D8661B88CF80EF802D57E2F"
                       & "7CEBCF1E00E083848BE19929C61B4237",
                     Expect_OKM =>
                       "E597B4027A756773E339650D68413E10"
                       & "C572998DC532340BFAF2289E8714EF126"
                       & "9FB32334B5F5E81BADBF00A4BEC7D4D3D"
                       & "11201832AF22B969E9E6D484FC9C3738C"
                       & "8168C47BCFE79F7F5A2B43E2D4F6993BB"
                       & "21A09BD8A72BBF1516C71EFAEABB638AB"
                       & "834");

      --  No salt, no info, L = 64 (salt is 64 zeros per RFC 5869).
      Check_HKDF512 (T,
                     Salt => "",
                     IKM  => "0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B",
                     Info => "",
                     No_Salt => True,
                     Expect_PRK =>
                       "FD200C4987AC491313BD4A2A13287121"
                       & "247239E11C9EF82802044B66EF357E5B"
                       & "194498D0682611382348572A7B1611DE"
                       & "54764094286320578A863F36562B0DF6",
                     Expect_OKM =>
                       "F5FA02B18298A72A8C23898A8703472C"
                       & "6EB179DC204C03425C970E3B164BF90F"
                       & "FF22D04836D0E2343BACC4E7CB6045FA"
                       & "AA698E0E3B3EB91331306DEF1DB8319E");
   end Test_HKDF_SHA512;

   --  ------------------------------
   --  Test the HKDF-SHA512 Derive_Key one-shot procedure.
   --  ------------------------------
   procedure Test_HKDF_SHA512_Derive_Key (T : in out Test) is
      Dec : constant Util.Encoders.Decoder := Util.Encoders.Create ("hex");
      Enc : constant Util.Encoders.Encoder := Util.Encoders.Create ("hex");
      Key : Util.Encoders.Secret_Key (Length => 32);
   begin
      Util.Encoders.KDF.HKDF_SHA512.Derive_Key
        (Salt   => Dec.Decode_Binary ("73616C74"),
         IKM    => Dec.Decode_Binary
           ("808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9F"),
         Info   => Dec.Decode_Binary ("7832353531392061656164206B6579"),
         Result => Key);
      Util.Tests.Assert_Equals (T,
                                "D2F83E9AF5C2C7C19FB65D40F419B1227"
                                & "0B1C843E92CE57749EF2CEB29A688A1",
                                Enc.Encode_Binary (Key.Secret),
                                "Invalid HKDF-SHA512 derived key");
   end Test_HKDF_SHA512_Derive_Key;

end Util.Encoders.KDF.Tests;
