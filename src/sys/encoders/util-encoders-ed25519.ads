-----------------------------------------------------------------------
--  util-encoders-ed25519 -- Ed25519 digital signature (RFC 8032) in pure Ada
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Util.Encoders.Random;

--  == Ed25519 digital signature ==
--  The `Util.Encoders.Ed25519` package implements the Ed25519 digital signature
--  algorithm defined by RFC 8032 (<i>Edwards-curve Digital Signature Algorithm
--  (EdDSA)</i>).  The implementation is written in pure Ada and does not depend
--  on any external cryptographic library: the SHA-512 hash function is
--  provided by the `Util.Encoders.SHA512` package and this package
--  provides the arithmetic over the prime field GF(2^255 - 19) and the
--  arithmetic over the edwards25519 curve group.
--
--  An Ed25519 private key is a 32-byte seed.  The public key is the 32-byte
--  encoding of the curve point `[a]B` where `a` is the clamped
--  SHA-512 hash of the seed and `B` is the standard base point.
--  A signature is a 64-byte array made of the encoded point `R`
--  followed by the 32-byte little-endian encoding of the scalar `S`.
--
--  All the keys, messages and signatures are exchanged as arrays of
--  `Ada.Streams.Stream_Element` so that any binary content can be
--  signed or verified.
--
--  The creation of an Ed25519 private key can be made by using a secure
--  random generator as follows (See Secure random generator).
--
--    Key : Util.Encoders.Ed25519.Private_Key;
--    ...
--       Util.Encoders.Ed25519.Create_Private_Key (Gen);
--
--  And from the private key, we can obtain the public key:
--
--    Pub_Key : Util.Encoders.Ed25519.Public_Key
--       := Util.Encoders.Ed25519.Get_Public_Key (Key);
--
--  The signature is produced by the `Sign` function with the private key
--  and it returns the 64-bytes signature.
--
--    S : Util.Encoders.Ed25519.Signature
--        := Util.Encoders.Ed25519.Sign (Pkey, Data);
--
--  And the verification is made by the `Verify` function with the public key:
--
--    Ok : Boolean := Util.Encoders.Ed25519.Verify (S, Data, Pub_Key);
package Util.Encoders.Ed25519 with Preelaborate => False is

   --  Number of bytes of a private key (the seed) or of a public key.
   Key_Size : constant := 32;

   --  Number of bytes of an Ed25519 signature.
   Signature_Size : constant := 64;

   --  An Ed25519 private key: a 32-byte seed.
   subtype Private_Key is Util.Encoders.Secret_Key (Length => Key_Size);

   --  An Ed25519 public key: the 32-byte encoding of the point [a]B.
   subtype Public_Key is Ada.Streams.Stream_Element_Array (1 .. Key_Size);

   --  An Ed25519 signature: R (32 bytes) followed by S (32 bytes).
   subtype Signature is Ada.Streams.Stream_Element_Array (1 .. Signature_Size);

   --  Create a new random Ed25519 private key (a random 32-byte seed) using
   --  the given random generator.
   procedure Create_Private_Key (Gen  : in out Util.Encoders.Random.Generator'Class;
                                 Seed : out Private_Key);

   --  Derive the public key associated with the private key seed.
   --  The public key is the encoding of the point <tt>[a]B</tt> where
   --  <tt>a</tt> is the clamped SHA-512 hash of the seed.
   function Get_Public_Key (Seed : in Private_Key) return Public_Key;

   --  Sign the message with the Ed25519 private key (RFC 8032, section 5.1.6).
   --  Returns the 64-byte signature of the message.
   function Sign (Seed : in Private_Key;
                  Message : in Ada.Streams.Stream_Element_Array) return Signature;

   --  Verify the Ed25519 signature of the message with the public key
   --  (RFC 8032, section 5.1.7).  Returns True when the signature is valid
   --  for the message and the public key, False otherwise (including when
   --  the signature does not have the expected 64-byte length).
   function Verify (Public    : in Public_Key;
                    Message   : in Ada.Streams.Stream_Element_Array;
                    Signature : in Ada.Streams.Stream_Element_Array) return Boolean;

end Util.Encoders.Ed25519;
