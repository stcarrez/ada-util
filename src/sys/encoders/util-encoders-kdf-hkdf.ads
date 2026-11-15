-----------------------------------------------------------------------
--  util-encoders-kdf-hkdf -- HMAC-based Extract-and-Expand KDF, RFC 5869
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;

--  === HMAC-based Extract-and-Expand Key Derivation Function ===
--  The `Util.Encoders.KDF.HKDF` generic package implements the HKDF key
--  derivation function defined by RFC 5869.  It is composed of two steps:
--
--  * <b>Extract</b> (RFC 5869 section 2.2): the input keying material (IKM) is
--    mixed with an optional salt to produce a fixed-length pseudorandom key:
--    <tt>PRK = HMAC-Hash (salt, IKM)</tt>.  When no salt is given, a string of
--    HashLen zeros is used.
--  * <b>Expand</b> (RFC 5869 section 2.3): the pseudorandom key is expanded into
--    L bytes of output keying material:
--    <tt>T(0) = empty, T(i) = HMAC-Hash (PRK, T(i-1) || info || i),
--        OKM = first L bytes of T(1) || T(2) || ...</tt>
--
--  The generic package is instantiated with an HMAC function and the hash
--  output length.  Instantiations with HMAC-SHA512 and HMAC-SHA256 are provided
--  by the `Util.Encoders.KDF.HKDF_SHA512` and `Util.Encoders.KDF.HKDF_SHA256`
--  packages.  A custom instantiation is done as follows:
--
--    package HKDF is new Util.Encoders.KDF.HKDF
--       (Length => Util.Encoders.HMAC.SHA512.HASH_SIZE,
--        HMAC   => Util.Encoders.HMAC.SHA512.Sign);
--
--  and a key is derived by using:
--
--    Key : Util.Encoders.Secret_Key (Length => 32);
--    Util.Encoders.KDF.HKDF_SHA512.Derive_Key (Salt   => Salt,
--                                              IKM    => Shared_Secret,
--                                              Info   => "my application",
--                                              Result => Key);
generic
   --  The length in bytes of the HMAC output (HashLen in RFC 5869).
   Length : Ada.Streams.Stream_Element_Offset;
   with procedure HMAC (Key  : in Ada.Streams.Stream_Element_Array;
                         Data : in Ada.Streams.Stream_Element_Array;
                         Into : out Ada.Streams.Stream_Element_Array);
package Util.Encoders.KDF.HKDF is

   --  HKDF-Extract (RFC 5869 section 2.2): <tt>PRK = HMAC-Hash (salt, IKM)</tt>.
   --  <b>PRK</b> must have a length equal to the hash length.
   procedure Extract (Salt : in Ada.Streams.Stream_Element_Array;
                      IKM  : in Ada.Streams.Stream_Element_Array;
                      PRK  : out Ada.Streams.Stream_Element_Array) with
      Pre => PRK'Length = Length;

   --  HKDF-Extract without salt: the salt is a string of HashLen zeros
   --  (RFC 5869 section 2.2).
   procedure Extract (IKM : in Ada.Streams.Stream_Element_Array;
                      PRK : out Ada.Streams.Stream_Element_Array) with
      Pre => PRK'Length = Length;

   --  HKDF-Expand (RFC 5869 section 2.3): expand the pseudorandom key <b>PRK</b>
   --  into <b>OKM</b> of at most 255 * HashLen bytes, optionally binding the
   --  <b>Info</b> application-specific information.
   procedure Expand (PRK  : in Ada.Streams.Stream_Element_Array;
                     Info : in Ada.Streams.Stream_Element_Array;
                     OKM  : out Ada.Streams.Stream_Element_Array) with
      Pre => PRK'Length = Length and then OKM'Length <= 255 * Length;

   --  HKDF extract then expand: derive the key <b>Result</b> from the input
   --  keying material <b>IKM</b>, the optional <b>Salt</b> and the optional
   --  application-specific <b>Info</b>.
   procedure Derive_Key (Salt   : in Ada.Streams.Stream_Element_Array;
                         IKM    : in Ada.Streams.Stream_Element_Array;
                         Info   : in Ada.Streams.Stream_Element_Array;
                         Result : out Secret_Key) with
      Pre => Result.Length <= 255 * Length;

end Util.Encoders.KDF.HKDF;
