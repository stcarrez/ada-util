-----------------------------------------------------------------------
--  util-encoders-crc32 -- CRC32
--  Copyright (C) 2025-2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Encoders.CRC32 is

   use Interfaces;

   --  The initial CRC value which is also the final XOR value (CRC-32/ISO-HDLC).
   CRC32_Init : constant Unsigned_32 := 16#FFFF_FFFF#;

   --  ------------------------------
   --  Initialize the context and build the CRC32 table for the given
   --  reflected polynomial.
   --  ------------------------------
   procedure Init (E    : in out Context;
                   Poly : in Unsigned_32 := CRC32_Poly) is
      Value : Unsigned_32;
   begin
      E.Value := CRC32_Init;
      for I in 0 .. 255 loop
         Value := Unsigned_32 (I);
         for J in 0 .. 7 loop
            if (Value and 1) /= 0 then
               Value := Shift_Right (Value, 1) xor Poly;
            else
               Value := Shift_Right (Value, 1);
            end if;
         end loop;
         E.Crc32 (I) := Value;
      end loop;
   end Init;

   --  ------------------------------
   --  Update the crc32 with the string.
   --  ------------------------------
   procedure Update (E : in out Context;
                     S : in String) is
      CRC : Unsigned_32 := E.Value;
   begin
      for C of S loop
         CRC := Shift_Right (CRC, 8)
           xor E.Crc32 (Natural ((CRC xor Unsigned_32 (Character'Pos (C))) and 16#FF#));
      end loop;
      E.Value := CRC;
   end Update;

   --  ------------------------------
   --  Update the crc32 with the array.
   --  ------------------------------
   procedure Update (E : in out Context;
                     S : in Ada.Streams.Stream_Element_Array) is
      CRC : Unsigned_32 := E.Value;
   begin
      for Byte of S loop
         CRC := Shift_Right (CRC, 8)
           xor E.Crc32 (Natural ((CRC xor Unsigned_32 (Byte)) and 16#FF#));
      end loop;
      E.Value := CRC;
   end Update;

   --  ------------------------------
   --  Computes the CRC32 value.
   --  ------------------------------
   procedure Finish (E      : in out Context;
                     Result : out Digest) is
   begin
      Result := E.Value xor CRC32_Init;
      E.Value := 16#FFFF_FFFF#;
   end Finish;

end Util.Encoders.CRC32;
