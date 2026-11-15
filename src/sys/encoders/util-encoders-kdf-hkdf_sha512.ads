-----------------------------------------------------------------------
--  util-encoders-kdf-hkdf_sha512 -- HKDF with HMAC-SHA512, RFC 5869
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Encoders.KDF.HKDF;
with Util.Encoders.HMAC.SHA512;

--  The <b>Util.Encoders.KDF.HKDF_SHA512</b> package is the instantiation of the
--  RFC 5869 HKDF key derivation function with HMAC-SHA512.
package Util.Encoders.KDF.HKDF_SHA512 is
  new Util.Encoders.KDF.HKDF (Length => Util.Encoders.HMAC.SHA512.HASH_SIZE,
                              HMAC   => Util.Encoders.HMAC.SHA512.Sign);
