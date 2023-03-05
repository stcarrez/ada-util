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

--  ------------------------------
--  HMAC-based One-Time Password, RFC 4226.  The counter value encoded
--  in big-endiang as 8-bytes is hashed using the given algorithm.  Then
--  the HMAC result is truncated to keep a 31-bit value (See RFC 4226 for
--  the truncation algorithm) and `Count` digits are kept from the 31-bit
--  value and returned in `Result`.
--  ------------------------------
function Util.Encoders.HMAC.HOTP (Secret  : in Secret_Key;
                                   Counter : in Interfaces.Unsigned_64;
                                   Count   : in Positive) return Natural is
   use Interfaces;

   function To_Unsigned_32 (Data   : in Stream_Element_Array;
                            Offset : in Stream_Element_Offset) return Unsigned_32;
   pragma Inline_Always (To_Unsigned_32);

   function To_Unsigned_32 (Data   : in Stream_Element_Array;
                            Offset : in Stream_Element_Offset) return Unsigned_32 is
   begin
      return Shift_Left (Unsigned_32 (Data (Offset)), 24) or
        Shift_Left (Unsigned_32 (Data (Offset + 1)), 16) or
        Shift_Left (Unsigned_32 (Data (Offset + 2)), 8) or
        Unsigned_32 (Data (Offset + 3));
   end To_Unsigned_32;

   Data   : Ada.Streams.Stream_Element_Array (1 .. 8);
   Val    : Interfaces.Unsigned_64 := Counter;
   Into   : Ada.Streams.Stream_Element_Array (1 .. Length);
   Offset : Ada.Streams.Stream_Element_Offset;
   T      : Unsigned_32;
   D      : Unsigned_32;
begin
   --  Build the data to hash and generate it.
   for I in reverse Data'Range loop
      Data (I) := Stream_Element (Val and 16#0ff#);
      Val := Interfaces.Shift_Right (Val, 8);
   end loop;
   Hash (Secret.Secret, Data, Into);

   --  Truncate the hash to get the 31-bit value.
   Offset := 1 + Stream_Element_Offset (Into (Into'Last) and 16#0F#);
   T := To_Unsigned_32 (Into, Offset) and 16#7fffffff#;

   --  Keep only Count digits from that 31-bit value.
   D := 10;
   for I in 1 .. Count - 1 loop
      D := D * 10;
   end loop;
   return Natural (T mod D);
end Util.Encoders.HMAC.HOTP;
