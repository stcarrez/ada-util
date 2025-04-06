# Encoders
The `Util.Encoders` package defines the `Encoder` and `Decoder` types
which provide a mechanism to transform a stream from one format into
another format.  The basic encoder and decoder support `base16`,
`base32`, `base64`, `base64url` and `sha1`.
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


