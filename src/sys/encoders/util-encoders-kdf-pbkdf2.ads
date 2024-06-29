-----------------------------------------------------------------------
--  util-encoders-kdf-pbkdf2 -- Password-Based Key Derivation Function 2, RFC 8018.
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  Password-Based Key Derivation Function 2, RFC 8018.
generic
   Length : Stream_Element_Offset;
   with procedure Hash (Key  : in Ada.Streams.Stream_Element_Array;
                        Data : in Ada.Streams.Stream_Element_Array;
                        Into : out Ada.Streams.Stream_Element_Array);
procedure Util.Encoders.KDF.PBKDF2 (Password : in Secret_Key;
                                    Salt     : in Secret_Key;
                                    Counter  : in Positive;
                                    Result   : out Secret_Key);
