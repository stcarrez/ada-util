-----------------------------------------------------------------------
--  util-encoders-kdf-pbkdf2 -- Password-Based Key Derivation Function 2, RFC 8018.
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  === Password-based key derivation function 2 ===
--  The `Util.Encoders.KDF.PBKDF2` generic procedure can be used to generate
--  a secure key from a password.  It implements the key derivative function
--  Password-Based Key Derivation Function 2, RFC 8018.
--  The generic procedure is instantiated with a `Hash` function which is
--  typically a `HMAC-SHA256` function.  An instantiation with such function
--  is provided by the `Util.Encoders.KDF.PBKDF2_HMAC_SHA256` procedure.
--  After instantiation, the procedure gets the password, a salt and
--  an iteration counter and it produces the derived key.
--
--    Pkey : constant Util.Encoders.Secret_Key
--       := Util.Encoders.Create (Password);
--    Salt : constant Util.Encoders.Secret_Key
--       := Util.Encoders.Create ("fakesalt");
--    Key  : Util.Encoders.Secret_Key (Length => AES.AES_256_Length
--   ...
--      Util.Encoders.KDF.PBKDF2_HMAC_SHA256
--         (Password => Pkey,
--          Salt     => Salt,
--          Counter  => 2000000,
--          Result   => Key);
generic
   Length : Stream_Element_Offset;
   with procedure Hash (Key  : in Ada.Streams.Stream_Element_Array;
                        Data : in Ada.Streams.Stream_Element_Array;
                        Into : out Ada.Streams.Stream_Element_Array);
procedure Util.Encoders.KDF.PBKDF2 (Password : in Secret_Key;
                                    Salt     : in Secret_Key;
                                    Counter  : in Positive;
                                    Result   : out Secret_Key);
