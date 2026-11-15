-----------------------------------------------------------------------
--  util-encoders-x25519 -- X25519 key exchange (RFC 7748)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Util.Encoders.Random;

--  == X25519 key exchange ==
--  The `Util.Encoders.X25519` package implements the X25519 key agreement
--  defined by RFC 7748 (<i>Elliptic Curves for Security</i>).  It can be used
--  with an HKDF algorithm for authentication with AES or ChaCha20.
--
--  The X25519 function computes a 32-byte shared secret from a 32-byte
--  private scalar and a 32-byte public u-coordinate on Curve25519:
--
--  * `Get_Public_Key` derives the public key of a private key,
--  * `Shared_Secret` computes the Diffie-Hellman shared secret between
--    a private key and the peer public key.
--
--  The creation of an X25519 private key can be made by using a secure
--  random generator as follows (See Secure random generator).
--
--    Key : Util.Encoders.X25519.Private_Key;
--    ...
--       Util.Encoders.X25519.Create_Private_Key (Gen, Key);
--
--  And from the private key, we can obtain the public key:
--
--    Pub_Key : Util.Encoders.X25519.Public_Key
--       := Util.Encoders.X25519.Get_Public_Key (Key);
--
--  Knowing the peer public key, the Diffie-Hellman shared secret
--  can be computed as follows:
--
--    Peer_Pub_Key : Util.Encoders.X25519.Public_Key := ...;
--    Secret : Util.Encoders.X25519.Shared_Key;
--      Util.Encoders.X25519.Shared_Secret (Key, Peer_Pub_Key, Secret);
--
package Util.Encoders.X25519 is

   --  Number of bytes of a key: private key, public key or shared secret.
   Key_Size : constant := 32;

   --  A 32-byte X25519 key (private scalar, public u-coordinate or shared secret).
   subtype Key is Util.Encoders.Secret_Key (Length => Key_Size);

   --  An X25519 private key: a 32-byte secret scalar.
   subtype Private_Key is Util.Encoders.Secret_Key (Length => Key_Size);

   --  An X25519 public key: the 32-byte u-coordinate of the public point.
   subtype Public_Key is Ada.Streams.Stream_Element_Array (1 .. Key_Size);

   --  An X25519 shared secret: the 32-byte x-coordinate of the shared point.
   --  subtype Shared_Key is Key;
   subtype Shared_Key is Util.Encoders.Secret_Key (Length => Key_Size);

   --  The raw X25519 function of RFC 7748: compute the u-coordinate of
   --  [scalar]P where P has the u-coordinate <tt>U</tt>.  The scalar is
   --  clamped as specified by RFC 7748 before the scalar multiplication.
   procedure X25519 (Scalar : in Key;
                     U      : in Public_Key;
                     Secret : out Shared_Key);

   --  Create a new random X25519 private key (32 random bytes) using the
   --  given random generator.  The scalar is clamped as specified by RFC 7748
   --  when the key is used.
   procedure Create_Private_Key (Gen : in out Util.Encoders.Random.Generator'Class;
                                 Key : out Private_Key);

   --  Derive the public key associated with the private key:
   --  `X25519 (Priv, 9)`.
   function Get_Public_Key (Priv : in Private_Key) return Public_Key;

   --  Compute the Diffie-Hellman shared secret between the private key and
   --  the peer public key: `X25519 (Priv, Peer_Public)`.
   procedure Shared_Secret (Priv        : in Private_Key;
                            Peer_Public : in Public_Key;
                            Secret      : out Shared_Key);

end Util.Encoders.X25519;
