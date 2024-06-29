-----------------------------------------------------------------------
--  util-encoders-kdf-pbkdf2 -- Password-Based Key Derivation Function 2, RFC 8018.
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Encoders.KDF.PBKDF2;
with Util.Encoders.HMAC.SHA256;
procedure Util.Encoders.KDF.PBKDF2_HMAC_SHA256 is
  new Util.Encoders.KDF.PBKDF2 (Length => Util.Encoders.HMAC.SHA256.HASH_SIZE,
                                Hash   => Util.Encoders.HMAC.SHA256.Sign);
