-----------------------------------------------------------------------
--  util-encoders-kdf-hotp -- HMAC-based One-Time Password, RFC 4226.
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  HMAC-based One-Time Password, RFC 4226.  The counter value encoded
--  in big-endiang as 8-bytes is hashed using the given algorithm.  Then
--  the HMAC result is truncated to keep a 31-bit value (See RFC 4226 for
--  the truncation algorithm) and `Count` digits are kept from the 31-bit
--  value and returned in `Result`.
generic
   Length : Stream_Element_Offset;
   with procedure Hash (Key  : in Ada.Streams.Stream_Element_Array;
                        Data : in Ada.Streams.Stream_Element_Array;
                        Into : out Ada.Streams.Stream_Element_Array);
function Util.Encoders.HMAC.HOTP (Secret  : in Secret_Key;
                                  Counter : in Interfaces.Unsigned_64;
                                  Count   : in Positive) return Natural;
