-----------------------------------------------------------------------
--  util-encoders-random -- Random number generator interface
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Encoders.Random.Pseudo is

   use Interfaces;

   overriding
   procedure Generate (Gen  : in out Pseudo_Generator;
                       Into : out Ada.Streams.Stream_Element_Array) is
      Size   : constant Ada.Streams.Stream_Element_Offset := Into'Length / 4;
      Remain : constant Ada.Streams.Stream_Element_Offset := Into'Length mod 4;
      Value  : Unsigned_32;
   begin
      --  Generate the random sequence (fill 32-bits at a time for each random call).
      for I in 0 .. Size - 1 loop
         Value := Id_Random.Random (Gen.Rand);
         Into (Into'First + 4 * I)     := Stream_Element (Value and 16#0FF#);
         Into (Into'First + 4 * I + 1) := Stream_Element (Shift_Right (Value, 8) and 16#0FF#);
         Into (Into'First + 4 * I + 2) := Stream_Element (Shift_Right (Value, 16) and 16#0FF#);
         Into (Into'First + 4 * I + 3) := Stream_Element (Shift_Right (Value, 24) and 16#0FF#);
      end loop;

      --  Fill the remaining bytes.
      if Remain > 0 then
         Value := Id_Random.Random (Gen.Rand);
         for I in 0 .. Remain - 1 loop
            Into (Into'Last - I) := Stream_Element (Value and 16#0FF#);
            Value := Shift_Right (Value, 8);
         end loop;
      end if;
   end Generate;

end Util.Encoders.Random.Pseudo;
