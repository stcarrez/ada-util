-----------------------------------------------------------------------
--  util-encoders-ecc -- Error Correction Code
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
package body Util.Encoders.ECC is

   use Interfaces;

   type Byte_Table is array (Interfaces.Unsigned_8 range 0 .. 255) of Interfaces.Unsigned_8;

   type Parity_Table is array (Ada.Streams.Stream_Element) of Interfaces.Unsigned_8;

   Parity : constant Parity_Table
     := (16#FF#, 16#D4#, 16#D2#, 16#F9#, 16#CC#, 16#E7#, 16#E1#, 16#CA#,
         16#CA#, 16#E1#, 16#E7#, 16#CC#, 16#F9#, 16#D2#, 16#D4#, 16#FF#,
         16#B4#, 16#9F#, 16#99#, 16#B2#, 16#87#, 16#AC#, 16#AA#, 16#81#,
         16#81#, 16#AA#, 16#AC#, 16#87#, 16#B2#, 16#99#, 16#9F#, 16#B4#,
         16#B2#, 16#99#, 16#9F#, 16#B4#, 16#81#, 16#AA#, 16#AC#, 16#87#,
         16#87#, 16#AC#, 16#AA#, 16#81#, 16#B4#, 16#9F#, 16#99#, 16#B2#,
         16#F9#, 16#D2#, 16#D4#, 16#FF#, 16#CA#, 16#E1#, 16#E7#, 16#CC#,
         16#CC#, 16#E7#, 16#E1#, 16#CA#, 16#FF#, 16#D4#, 16#D2#, 16#F9#,
         16#AC#, 16#87#, 16#81#, 16#AA#, 16#9F#, 16#B4#, 16#B2#, 16#99#,
         16#99#, 16#B2#, 16#B4#, 16#9F#, 16#AA#, 16#81#, 16#87#, 16#AC#,
         16#E7#, 16#CC#, 16#CA#, 16#E1#, 16#D4#, 16#FF#, 16#F9#, 16#D2#,
         16#D2#, 16#F9#, 16#FF#, 16#D4#, 16#E1#, 16#CA#, 16#CC#, 16#E7#,
         16#E1#, 16#CA#, 16#CC#, 16#E7#, 16#D2#, 16#F9#, 16#FF#, 16#D4#,
         16#D4#, 16#FF#, 16#F9#, 16#D2#, 16#E7#, 16#CC#, 16#CA#, 16#E1#,
         16#AA#, 16#81#, 16#87#, 16#AC#, 16#99#, 16#B2#, 16#B4#, 16#9F#,
         16#9F#, 16#B4#, 16#B2#, 16#99#, 16#AC#, 16#87#, 16#81#, 16#AA#,
         16#AA#, 16#81#, 16#87#, 16#AC#, 16#99#, 16#B2#, 16#B4#, 16#9F#,
         16#9F#, 16#B4#, 16#B2#, 16#99#, 16#AC#, 16#87#, 16#81#, 16#AA#,
         16#E1#, 16#CA#, 16#CC#, 16#E7#, 16#D2#, 16#F9#, 16#FF#, 16#D4#,
         16#D4#, 16#FF#, 16#F9#, 16#D2#, 16#E7#, 16#CC#, 16#CA#, 16#E1#,
         16#E7#, 16#CC#, 16#CA#, 16#E1#, 16#D4#, 16#FF#, 16#F9#, 16#D2#,
         16#D2#, 16#F9#, 16#FF#, 16#D4#, 16#E1#, 16#CA#, 16#CC#, 16#E7#,
         16#AC#, 16#87#, 16#81#, 16#AA#, 16#9F#, 16#B4#, 16#B2#, 16#99#,
         16#99#, 16#B2#, 16#B4#, 16#9F#, 16#AA#, 16#81#, 16#87#, 16#AC#,
         16#F9#, 16#D2#, 16#D4#, 16#FF#, 16#CA#, 16#E1#, 16#E7#, 16#CC#,
         16#CC#, 16#E7#, 16#E1#, 16#CA#, 16#FF#, 16#D4#, 16#D2#, 16#F9#,
         16#B2#, 16#99#, 16#9F#, 16#B4#, 16#81#, 16#AA#, 16#AC#, 16#87#,
         16#87#, 16#AC#, 16#AA#, 16#81#, 16#B4#, 16#9F#, 16#99#, 16#B2#,
         16#B4#, 16#9F#, 16#99#, 16#B2#, 16#87#, 16#AC#, 16#AA#, 16#81#,
         16#81#, 16#AA#, 16#AC#, 16#87#, 16#B2#, 16#99#, 16#9F#, 16#B4#,
         16#FF#, 16#D4#, 16#D2#, 16#F9#, 16#CC#, 16#E7#, 16#E1#, 16#CA#,
         16#CA#, 16#E1#, 16#E7#, 16#CC#, 16#F9#, 16#D2#, 16#D4#, 16#FF#);

   Bit_Count_Table : constant Byte_Table
     := (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
         1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
         1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
         2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
         1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
         2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
         2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
         3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
         1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
         2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
         2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
         3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
         2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
         3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
         3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
         4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

   function Count (Data : Ada.Streams.Stream_Element) return Natural;

   --  Make the 3 bytes ECC code that corresponds to the data array.
   procedure Make (Data : in Ada.Streams.Stream_Element_Array;
                   Code : out ECC_Code) is
      LP0, LP1, LP2, LP3, LP4, LP5, LP6, LP7       : Ada.Streams.Stream_Element := 0;
      LP8, LP9, LP10, LP11, LP12, LP13, LP14, LP15 : Ada.Streams.Stream_Element := 0;
      LP16, LP17 : Ada.Streams.Stream_Element := 0;
      I : Unsigned_16 := 0;
   begin
      for D of Data loop
         if (I and 16#01#) /= 0 then
            LP1 := LP1 xor D;
         else
            LP0 := LP0 xor D;
         end if;
         if (I and 16#02#) /= 0 then
            LP3 := LP3 xor D;
         else
            LP2 := LP2 xor D;
         end if;
         if (I and 16#04#) /= 0 then
            LP5 := LP5 xor D;
         else
            LP4 := LP4 xor D;
         end if;
         if (I and 16#08#) /= 0 then
            LP7 := LP7 xor D;
         else
            LP6 := LP6 xor D;
         end if;
         if (I and 16#10#) /= 0 then
            LP9 := LP9 xor D;
         else
            LP8 := LP8 xor D;
         end if;
         if (I and 16#20#) /= 0 then
            LP11 := LP11 xor D;
         else
            LP10 := LP10 xor D;
         end if;
         if (I and 16#40#) /= 0 then
            LP13 := LP13 xor D;
         else
            LP12 := LP12 xor D;
         end if;
         if (I and 16#80#) /= 0 then
            LP15 := LP15 xor D;
         else
            LP14 := LP14 xor D;
         end if;
         if Data'Length = 512 then
            if (I and 16#100#) /= 0 then
               LP17 := LP17 xor D;
            else
               LP16 := LP16 xor D;
            end if;
         end if;
         I := I + 1;
      end loop;
      Code (0) := Stream_Element ((Parity (LP0) and 16#01#)
                                  or Shift_Left (Parity (LP1) and 16#01#, 1)
                                  or Shift_Left (Parity (LP2) and 16#01#, 2)
                                  or Shift_Left (Parity (LP3) and 16#01#, 3)
                                  or Shift_Left (Parity (LP4) and 16#01#, 4)
                                  or Shift_Left (Parity (LP5) and 16#01#, 5)
                                  or Shift_Left (Parity (LP6) and 16#01#, 6)
                                  or Shift_Left (Parity (LP7) and 16#01#, 7));
      Code (1) := Stream_Element ((Parity (LP8) and 16#01#)
                                  or Shift_Left (Parity (LP9) and 16#01#, 1)
                                  or Shift_Left (Parity (LP10) and 16#01#, 2)
                                  or Shift_Left (Parity (LP11) and 16#01#, 3)
                                  or Shift_Left (Parity (LP12) and 16#01#, 4)
                                  or Shift_Left (Parity (LP13) and 16#01#, 5)
                                  or Shift_Left (Parity (LP14) and 16#01#, 6)
                                  or Shift_Left (Parity (LP15) and 16#01#, 7));

      Code (2) := Stream_Element (Shift_Left (Parity (LP15 xor LP14) and 16#fe#, 1)
                                  or Shift_Left (Parity (LP17) and 16#01#, 1)
                                  or (Parity (LP16) and 16#01#));
   end Make;

   function Count (Data : Ada.Streams.Stream_Element) return Natural is
   begin
      return Natural (Bit_Count_Table (Interfaces.Unsigned_8 (Data)));
   end Count;

   --  ------------------------------
   --  Check and correct the data array according to the expected ECC codes and current codes.
   --  At most one bit can be fixed and two error bits can be detected.
   --  ------------------------------
   function Correct (Data         : in out Ada.Streams.Stream_Element_Array;
                     Expect_Code  : in ECC_Code;
                     Current_Code : in ECC_Code) return ECC_Result is

      Check   : ECC_Code;
      Bits    : Natural;
      Bit_Pos : Natural;
      Pos     : Unsigned_16;
   begin
      Check (0) := Expect_Code (0) xor Current_Code (0);
      Check (1) := Expect_Code (1) xor Current_Code (1);
      Check (2) := Expect_Code (2) xor Current_Code (2);
      if (Check (0) or Check (1) or Check (2)) = 0 then
         return NO_ERROR;
      end if;
      Bits := Count (Check (0)) + Count (Check (1)) + Count (Check (2));
      if (Bits = 11 and then Data'Length = 256)
        or else (Bits = 12 and then Data'Length = 512)
      then
         Bit_Pos := Natural ((Shift_Right (Unsigned_8 (Check (2)), 3) and 16#01#)
                             or (Shift_Right (Unsigned_8 (Check (2)), 4) and 16#02#)
                             or (Shift_Right (Unsigned_8 (Check (2)), 5) and 16#04#));
         Pos := ((Shift_Right (Unsigned_16 (Check (0)), 1) and 16#01#)
                 or (Shift_Right (Unsigned_16 (Check (0)), 2) and 16#02#)
                 or (Shift_Right (Unsigned_16 (Check (0)), 3) and 16#04#)
                 or (Shift_Right (Unsigned_16 (Check (0)), 4) and 16#08#)
                 or (Shift_Left (Unsigned_16 (Check (1)), 3) and 16#10#)
                 or (Shift_Left (Unsigned_16 (Check (1)), 2) and 16#20#)
                 or (Shift_Left (Unsigned_16 (Check (1)), 1) and 16#40#)
                 or (Unsigned_16 (Check (1)) and 16#80#));
         if Data'Length = 512 then
            Pos := Pos + (Shift_Left (Unsigned_16 (Check (2)), 7) and 16#100#);
         end if;
         Data (Stream_Element_Offset (Pos))
           := Data (Stream_Element_Offset (Pos))
           xor Stream_Element (Shift_Left (Unsigned_8 (1), Bit_Pos));
         return CORRECTABLE_ERROR;
      elsif Bits = 1 then
         return ECC_ERROR;
      else
         return UNCORRECTABLE_ERROR;
      end if;
   end Correct;

   --  ------------------------------
   --  Check and correct the data array according to the expected ECC codes and current codes.
   --  At most one bit can be fixed and two error bits can be detected.
   --  ------------------------------
   function Correct (Data        : in out Ada.Streams.Stream_Element_Array;
                     Expect_Code : in ECC_Code) return ECC_Result is
      Current_Code : ECC_Code;
   begin
      Make (Data, Current_Code);
      return Correct (Data, Expect_Code, Current_Code);
   end Correct;

end Util.Encoders.ECC;
