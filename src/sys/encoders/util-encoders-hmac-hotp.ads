-----------------------------------------------------------------------
--  util-encoders-kdf-hotp -- HMAC-based One-Time Password, RFC 4226.
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
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
