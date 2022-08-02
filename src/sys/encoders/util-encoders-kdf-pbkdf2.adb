-----------------------------------------------------------------------
--  util-encoders-kdf-pbkdf2 -- Password-Based Key Derivation Function 2, RFC 8018.
--  Copyright (C) 2019, 2022 Stephane Carrez
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
with Interfaces;
procedure Util.Encoders.KDF.PBKDF2 (Password : in Secret_Key;
                                    Salt     : in Secret_Key;
                                    Counter  : in Positive;
                                    Result   : out Secret_Key) is
   use Interfaces;

   Total : constant Key_Length := Salt.Length + Length + 4;
   Pos   : constant Ada.Streams.Stream_Element_Offset := 1 + Salt.Secret'Last;
   First : Ada.Streams.Stream_Element_Offset := 1;
   I     : Unsigned_32 := 1;
   U     : Secret_Key (Total);
   Len   : Ada.Streams.Stream_Element_Offset;
begin
   --  RFC 8018, 5.2.  PBKDF2
   while First <= Result.Secret'Last loop
      --  Compute: U_1 = PRF (P, S || INT (i))
      U.Secret (Salt.Secret'Range) := Salt.Secret;
      U.Secret (Pos) := Stream_Element (Shift_Left (I, 24) and 16#0ff#);
      U.Secret (Pos + 1) := Stream_Element (Shift_Left (I, 16) and 16#0ff#);
      U.Secret (Pos + 2) := Stream_Element (Shift_Left (I, 8) and 16#0ff#);
      U.Secret (Pos + 3) := Stream_Element (I and 16#0ff#);
      Len := Result.Secret'Last - First + 1;
      if Len > Length then
         Len := Length;
      end if;
      Hash (Key  => Password.Secret,
            Data => U.Secret (1 .. Pos + 3),
            Into => U.Secret (1 .. Length));

      --  Compute: F (P, S, c, i) = U_1 \xor U_2 \xor ... \xor U_c
      --  With: U_c = PRF (P, U_{c-1}) .
      Result.Secret (First .. First + Len - 1) := U.Secret (1 .. Len);
      for C in 1 .. Counter - 1 loop
         --  Cancel the warning: writable actual for "Data" overlaps with actual for "Into".
         pragma Warnings (Off);

         Hash (Key  => Password.Secret,
               Data => U.Secret (1 .. Length),
               Into => U.Secret (1 .. Length));

         pragma Warnings (On);

         for J in 1 .. Len loop
            Result.Secret (First + J - 1) := Result.Secret (First + J - 1) xor U.Secret (J);
         end loop;
      end loop;
      I := I + 1;
      First := First + Length;
   end loop;
end Util.Encoders.KDF.PBKDF2;
