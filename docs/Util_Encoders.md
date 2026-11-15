# Encoders
The `Util.Encoders` package defines the `Encoder` and `Decoder` types
which provide a mechanism to transform a stream from one format into
another format.  The basic encoder and decoder support `base16`,
`base32`, `base62`, `base64`, `base64url` and `sha1`.
The following code extract will encode in `base64`:

```Ada
C : constant Encoder := Util.Encoders.Create ("base64");
S : constant String := C.Encode ("Ada is great!");
```

and the next code extract will decode the `base64`:

```Ada
D : constant Decoder := Util.Encoders.Create ("base64");
S : constant String := D.Decode ("QWRhIGlzIGdyZWF0IQ==");
```

To use the packages described here, use the following GNAT project:

```Ada
with "utilada_sys";
```

## URI Encoder and Decoder
The `Util.Encoders.URI` package provides operations to encode and decode
using the URI percent encoding and decoding scheme.
A string encoded using percent encoding as described in [RFC 3986](https://tools.ietf.org/html/rfc3986) is
simply decoded as follows:

```Ada
Decoded : constant String := Util.Encoders.URI.Decode ("%20%2F%3A");
```

To encode a string, one must choose the character set that must be encoded
and then call the `Encode` function.  The character set indicates those
characters that must be percent encoded.  Two character sets are provided,

* `HREF_STRICT` defines a strict character set to encode all reserved
  characters as defined by [RFC 3986](https://tools.ietf.org/html/rfc3986).  This is the default.
* `HREF_LOOSE` defines a character set that does not encode the
  reserved characters such as `-_.+!*'(),%#@?=;:/&$`.

```Ada
Encoded : constant String := Util.Encoders.URI.Encode (" /:");
```

## Secret keys
The `Util.Encoders` package defines the `Secret_Key` limited type which is
intended to be used to hold secret keys.  The secret itself can only be
accessed from the `Util.Encoders` packages and its children.  When the object
representing the key is destroyed, the memory that hold the key is cleared.
The `Secret_Key` is basically used for AES encryption but could be used for
other purposes.  The secret key can be created from a string as follows:

```Ada
Key : constant Util.Encoders.Secret_Key := Create ("password");
```

### Decoding
A secret key is a binary content that sometimes must be retrieved from another
format.  For example, it could be represented as a Base64 string.  To help
and reduce key leaks the `Decode_Key` function can be used.  The first step
is to obtain a `Decoder` object as described at begining:

```Ada
D : constant Decoder := Util.Encoders.Create ("base64");
K : constant Secret_Key := Util.Encoders.Decode_Key (D, "cGFzc3dvcmQ=");
```

### Decrypting keys
The secret key can also be obtained by using the `Decrypt_Secret` procedure
provided by the AES package.  The procedure only accepts a binary content
and to decrypt that key it is also necessary to know a first encryption key.

```Ada
Encrypted_Key : Ada.Streams.Stream_Element_Array := ...;
Decipher : Util.Encoders.AES.Decoder;
...
  Decipher.Set_Key (...);
  Decipher.Decrypt_Secret (Encrypted_Key, Key);
```

### Password-based key derivation function 2
The `Util.Encoders.KDF.PBKDF2` generic procedure can be used to generate
a secure key from a password.  It implements the key derivative function
Password-Based Key Derivation Function 2, [RFC 8018](https://tools.ietf.org/html/rfc8018).
The generic procedure is instantiated with a `Hash` function which is
typically a `HMAC-SHA256` function.  An instantiation with such function
is provided by the `Util.Encoders.KDF.PBKDF2_HMAC_SHA256` procedure.
After instantiation, the procedure gets the password, a salt and
an iteration counter and it produces the derived key.

```Ada
Pkey : constant Util.Encoders.Secret_Key
   := Util.Encoders.Create (Password);
Salt : constant Util.Encoders.Secret_Key
   := Util.Encoders.Create ("fakesalt");
Key  : Util.Encoders.Secret_Key (Length => AES.AES_256_Length
 ...
  Util.Encoders.KDF.PBKDF2_HMAC_SHA256
     (Password => Pkey,
      Salt     => Salt,
      Counter  => 2000000,
      Result   => Key);
```

### HMAC-based Extract-and-Expand Key Derivation Function
The `Util.Encoders.KDF.HKDF` generic package implements the HKDF key
derivation function defined by [RFC 5869](https://tools.ietf.org/html/rfc5869).  It is composed of two steps:

* <b>Extract</b> (RFC 5869 section 2.2): the input keying material (IKM) is
  mixed with an optional salt to produce a fixed-length pseudorandom key:
  <tt>PRK = HMAC-Hash (salt, IKM)</tt>.  When no salt is given, a string of
  HashLen zeros is used.
* <b>Expand</b> (RFC 5869 section 2.3): the pseudorandom key is expanded into
  L bytes of output keying material:
  <tt>T(0) = empty, T(i) = HMAC-Hash (PRK, T(i-1) || info || i),
  OKM = first L bytes of T(1) || T(2) || ...</tt>

The generic package is instantiated with an HMAC function and the hash
output length.  Instantiations with HMAC-SHA512 and HMAC-SHA256 are provided
by the `Util.Encoders.KDF.HKDF_SHA512` and `Util.Encoders.KDF.HKDF_SHA256`
packages.  A custom instantiation is done as follows:

```Ada
package HKDF is new Util.Encoders.KDF.HKDF
   (Length => Util.Encoders.HMAC.SHA512.HASH_SIZE,
    HMAC   => Util.Encoders.HMAC.SHA512.Sign);
```

and a key is derived by using:

```Ada
Key : Util.Encoders.Secret_Key (Length => 32);
Util.Encoders.KDF.HKDF_SHA512.Derive_Key (Salt   => Salt,
                                          IKM    => Shared_Secret,
                                          Info   => "my application",
                                          Result => Key);
```

## Random Generator
The `Util.Encoders.Random` package generates random bytes intended to be
used for a nonce, a secret key, a seed or an access token.

The `Generator` interface is implemented by:

* `Util.Encoders.Random.System.System_Generator`: reads the operating
  system random source (<tt>/dev/urandom</tt>); when it is not available,
  a Xorshiftr pseudo-random generator seeded with the clock is used,
* `Util.Encoders.Random.Pseudo.Pseudo_Generator`: a pseudo-random
  generator based on `Ada.Numerics.Discrete_Random` (not suitable
  for cryptographic use),
* `Util.Encoders.Random.Xorshiftr.Xorshiftr_Generator`: a fast
  xorshift128+ pseudo-random generator (not suitable for cryptographic
  use),
* `Util.Encoders.Random.Synchronized_Generator`: a generic package that
  wraps a generator in a protected type so that it is safe to share it
  between several tasks.

### System random generator
The `Util.Encoders.Random.System` package provides the `System_Generator`
type which reads the operating system random source (<tt>/dev/urandom</tt>).
When it is not available, the Xorshiftr pseudo-random generator seeded
with the clock is used.

### Xorshift+ random generator
The `Util.Encoders.Random.Xorshiftr` package provides the
`Xorshiftr_Generator` type implementing the `Generator` interface with the
xorshift128+ algorithm (Sebastiano Vigna, <i>An experimental exploration of
Marsaglia's xorshift generators, scrambled</i>, 2014).  The generator has a
128-bit state and a period of 2^128 - 1.  It is fast but <b>not</b> suitable
for cryptographic use; use `Util.Encoders.Random.Secure` for that.

### Secure random generator
The `Util.Encoders.Random.Secure` generic package transforms a basic random
generator into a cryptographically secure random generator, similar to
`arc4random` on recent NetBSD: a ChaCha20 stream cipher (RFC 8439) is seeded
with a fresh key and nonce drawn from the underlying <b>Random_Generator</b>,
and the generated bytes are the ChaCha20 keystream.  As long as the key is
not reused, the output is indistinguishable from random even if the
underlying generator is weak.

```Ada
package Secure_Random is
  new Util.Encoders.Random.Secure
    (Random_Generator => Util.Encoders.Random.Xsorshiftr.Xorshiftr_Generator);
Gen : Secure_Random.Secure_Generator;
...
Gen.Generate (Buf);
```

### Synchronized random generator
The `Util.Encoders.Random.Synchronized_Generator` generic package wraps a
random generator in a protected type so that the resulting `Generator` is
safe to share it between several tasks:

```Ada
package Sync_Generator is
  new Util.Encoders.Random.Xorshiftr
    (Random_Generator => Util.Encoders.Random.Xorshiftr.Xorshiftr_Generator);
Gen : Sync_Generator.Generator;

```

It is possible to access the generator itself from a procedure by calling
the `Configure` procedure:

```Ada
procedure Setup (Gen : in out Xorshiftr_Generator) is
begin
   Gen.Set_Seed (42);
end Setup;
Gen.Configure (Setup'Access);
```

## AES Encryption and decryption
The `Util.Encoders.AES` package implements the basis for AES encryption
and decryption used by the `Util.Streams.AES` package for AES streams.
Several encryption modes are available and described by the `AES_Mode`
type and the padding is controlled by the `AES_Padding` type.
The encryption is provided by methods on the `Encoder` type.  The `Set_Key`
procedure configures the encryption key and AES encryption mode.  The key
must be 16, 24 or 32 bytes as specified by the pre-condition.  Then, the
`Transform` procedure is called as needed to encrypt the content in a
destination buffer.  It returns the position of the last encoded byte as
well as the position of the last encrypted byte.  It can be called several
times if the data to encrypt does not fit in the destination buffer.
At the end, it is important to call the `Finish` procedure so that it
handles correctly the last AES block which may contain the last encrypted
bytes as well as optional padding.  An encryption function is shown below:

```Ada
function Encrypt (Key   : in Secret_Key;
                  Src   : in Stream_Element_Array) return Stream_Element_Array is
   Size    : constant Stream_Element_Offset := AES.Align (Src'Length);
   Cipher  : Util.Encoders.AES.Encoder;
   Encoded, Last : Stream_Element_Offset;
   Result  : Stream_Element_Array (1 .. Size);
begin
   Cipher.Set_Key (Key, AES.ECB);
   Cipher.Transform (Src, Result (Result'First .. Result'Last - 16),
                     Last, Encoded);
   Cipher.Finish (Result (Last + 1 .. Result'Last), Last);
   return Result;
end Encrypt;
```

With AES, the encryption result is always a multiple of AES block size (16 bytes).
The `Align` function allows to easily compute the expected encrypted size.

Decrypting a content follows the same principles but with the `Decoder` type.
The source array must be a multiple of AES block and the last AES block may
contain some padding.  The `Finish` procedure will then indicate the last
valid byte in the decrypted buffer.

```Ada
function Decrypt (Key   : in Secret_Key;
                  Src   : in Stream_Element_Array) return Stream_Element_Array is
   Size     : constant Stream_Element_Offset := Src'Length;
   Decipher : Util.Encoders.AES.Decoder;
   Result   : Stream_Element_Array (1 .. Size);
   Encoded, Last : Stream_Element_Offset;
begin
   Decipher.Set_Key (Key, AES.ECB);
   Decipher.Set_Padding (AES.PKCS7_PADDING);
   Decipher.Transform (Src, Result, Last, Encoded);
   Decipher.Finish (Result (Last + 1 .. Result'Last), Last);
   return Result (Result'First .. Last);
end Decrypt;
```

## ChaCha20 stream cipher
The `Util.Encoders.ChaCha20` package implements the ChaCha20 stream cipher
defined by [RFC 8439](https://tools.ietf.org/html/rfc8439).  The cipher uses a 256-bit key, a 96-bit nonce and a
32-bit block counter.  The `Block` procedure produces the 64-byte
keystream block and the `Crypt` procedure applies the keystream on a
buffer.

The `Encoder` and `Decoder` types implement the `Util.Encoders.Transformer`
interface (similar to AES) so that a content can be encrypted or decrypted
by calling the `Transform` procedure.  ChaCha20 is a stream cipher: the
encryption and the decryption are the same operation (XOR with the
keystream) and the output has the same length as the input (no padding).
The key is configured with the `Set_Key` procedure and the nonce with the
`Set_Nonce` procedure.  The nonce must be unique for each message encrypted
with the same key.

```Ada
Cipher : Util.Encoders.ChaCha20.Encoder;
...
Cipher.Set_Key (Key);
Cipher.Set_Nonce (Nonce12);
Cipher.Transform (Data, Encrypted, Last, Encoded);
```

## Poly1305 one-time authenticator
The `Util.Encoders.Poly1305` package implements the Poly1305 one-time
authenticator defined by [RFC 8439](https://tools.ietf.org/html/rfc8439).  Poly1305 computes a 128-bit tag of a
message with a 256-bit one-time key `r || s`: the first 16
bytes are clamped to form `r` and the last 16 bytes are `s`.

The key is a `one-time` key: it must never be reused for another
message, otherwise the authenticity is lost.

The `Context` type allows to compute the tag incrementally:

```Ada
Ctx : Util.Encoders.Poly1305.Context;
...
Util.Encoders.Poly1305.Set_Key (Ctx, Key32);
Util.Encoders.Poly1305.Update (Ctx, Part1);
Util.Encoders.Poly1305.Update (Ctx, Part2);
Util.Encoders.Poly1305.Finish (Ctx, Tag);
```

The `Mac` procedure computes the tag of a complete message in one call.

## AEAD ChaCha20-Poly1305
The `Util.Encoders.AEAD` package implements the AEAD (Authenticated Encryption
with Associated Data) construction of [RFC 8439](https://tools.ietf.org/html/rfc8439) combining the ChaCha20 stream
cipher with the Poly1305 one-time authenticator:

* the one-time Poly1305 key is the first 32 bytes of the ChaCha20 keystream
  block with counter 0,
* the message is encrypted with the keystream starting at counter 1,
* the 128-bit tag authenticates the optional AAD and the ciphertext:

```Ada
<tt>AAD || pad16 || ciphertext || pad16 || u64le(len(AAD))
                 || u64le(len(ciphertext))</tt>.
```

The `Encoder` and `Decoder` types implement the `Util.Encoders.Transformer`
interface (similar to AES and ChaCha20).  The `Encoder` streams the ciphertext
in `Transform` and appends the 16-byte tag in `Finish`.  The `Decoder` expects
the tag as the last 16 bytes of the input stream and verifies it in `Finish`;
it raises `Encoding_Error` when the tag is invalid.  The decrypted data written
by `Transform` must not be used before `Finish` has completed without error.

All the state is kept in the object itself: no buffer is dynamically allocated
and the data is copied only once from the input to the output array.

```Ada
Enc : Util.Encoders.AEAD.Encoder;
...
Enc.Set_Key (Key);
Enc.Set_Nonce (Nonce12);
Enc.Update_AAD (AAD);            --  optional, before Transform
Enc.Transform (Plain, Cipher, Last, Encoded);
Enc.Finish (Tag_Buf, Tag_Last);  --  16-byte tag
```

The nonce must be unique for each message encrypted with the same key.

## Ed25519 digital signature
The `Util.Encoders.Ed25519` package implements the Ed25519 digital signature
algorithm defined by [RFC 8032](https://tools.ietf.org/html/rfc8032) (<i>Edwards-curve Digital Signature Algorithm
(EdDSA)</i>).  The implementation is written in pure Ada and does not depend
on any external cryptographic library: the SHA-512 hash function is
provided by the `Util.Encoders.SHA512` package and this package
provides the arithmetic over the prime field GF(2^255 - 19) and the
arithmetic over the edwards25519 curve group.

An Ed25519 private key is a 32-byte seed.  The public key is the 32-byte
encoding of the curve point `[a]B` where `a` is the clamped
SHA-512 hash of the seed and `B` is the standard base point.
A signature is a 64-byte array made of the encoded point `R`
followed by the 32-byte little-endian encoding of the scalar `S`.

All the keys, messages and signatures are exchanged as arrays of
`Ada.Streams.Stream_Element` so that any binary content can be
signed or verified.

The creation of an Ed25519 private key can be made by using a secure
random generator as follows (See Secure random generator).

```Ada
Key : Util.Encoders.Ed25519.Private_Key;
...
   Util.Encoders.Ed25519.Create_Private_Key (Gen, Key);
```

And from the private key, we can obtain the public key:

```Ada
Pub_Key : Util.Encoders.Ed25519.Public_Key
   := Util.Encoders.Ed25519.Get_Public_Key (Key);
```

The signature is produced by the `Sign` function with the private key
and it returns the 64-bytes signature.

```Ada
S : Util.Encoders.Ed25519.Signature
    := Util.Encoders.Ed25519.Sign (Pkey, Data);
```

And the verification is made by the `Verify` function with the public key:

```Ada
Ok : Boolean := Util.Encoders.Ed25519.Verify (S, Data, Pub_Key);
```

## X25519 key exchange
The `Util.Encoders.X25519` package implements the X25519 key agreement
defined by [RFC 7748](https://tools.ietf.org/html/rfc7748) (<i>Elliptic Curves for Security</i>).  It can be used
with an HKDF algorithm for authentication with AES or ChaCha20.

The X25519 function computes a 32-byte shared secret from a 32-byte
private scalar and a 32-byte public u-coordinate on Curve25519:

* `Get_Public_Key` derives the public key of a private key,
* `Shared_Secret` computes the Diffie-Hellman shared secret between
  a private key and the peer public key.

The creation of an X25519 private key can be made by using a secure
random generator as follows (See Secure random generator).

```Ada
Key : Util.Encoders.X25519.Private_Key;
...
   Util.Encoders.X25519.Create_Private_Key (Gen, Key);
```

And from the private key, we can obtain the public key:

```Ada
Pub_Key : Util.Encoders.X25519.Public_Key
   := Util.Encoders.X25519.Get_Public_Key (Key);
```

Knowing the peer public key, the Diffie-Hellman shared secret
can be computed as follows:

```Ada
Peer_Pub_Key : Util.Encoders.X25519.Public_Key := ...;
Secret : Util.Encoders.X25519.Shared_Key;
  Util.Encoders.X25519.Shared_Secret (Key, Peer_Pub_Key, Secret);
```

## Error Correction Code
The `Util.Encoders.ECC` package provides operations to support error correction codes.
The error correction works on blocks of 256 or 512 bytes and can detect 2-bit errors
and correct 1-bit error.  The ECC uses only three additional bytes.
The ECC algorithm implemented by this package is implemented by several NAND Flash
memory.  It can be used to increase the robustness of data to bit-tempering when
the data is restored from an external storage (note that if the external storage has
its own ECC correction, adding another software ECC correction will probably not help).

The ECC code is generated by using the `Make` procedure that gets a block of 256 or
512 bytes and produces the 3 bytes ECC code.  The ECC code must be saved together with
the data block.

```Ada
Code : Util.Encoders.ECC.ECC_Code;
...
Util.Encoders.ECC.Make (Data, Code);
```

When reading the data block, you can verify and correct it by running again the
`Make` procedure on the data block and then compare the current ECC code with the
expected ECC code produced by the first call.  The `Correct` function is then called
with the data block, the expected ECC code that was saved with the data block and
the computed ECC code.

```Ada
New_Code : Util.Encoders.ECC.ECC_Code;
...
Util.Encoders.ECC.Make (Data, New_Code);
case Util.Encoders.ECC.Correct (Data, Expect_Code, New_Code) is
   when NO_ERROR | CORRECTABLE_ERROR => ...
   when others => ...
end case;
```

## CRC32
The `Util.Encoders.CRC32` package implements the standard CRC-32
checksum (CRC-32/ISO-HDLC) as used by zlib, gzip, ZIP, PNG and Ethernet:

- polynomial    : 16#04C11DB7# (reflected form: 16#EDB88320#)
- initial value : 16#FFFFFFFF#
- reflected input and output
-  final XOR     : 16#FFFFFFFF#

With these settings, the CRC-32 of the ASCII string "123456789" is 16#CBF43926#.
To compute the CRC32, create a context object and feed it with the content
by calling `Update`.  At the end, get the result by calling `Finish`:

```Ada
Context : Util.Encoders.CRC32.Context;
Digest  : Util.Encoders.Digest;
...
  Context.Update ("data...");
  Context.Finish (Digest);
```

If you want to use a different polynomial, call `Init` before feeding the
context:

```Ada
  Context.Init (16#82F6_3B78#); -- CRC-32C (Castagnoli)
```


