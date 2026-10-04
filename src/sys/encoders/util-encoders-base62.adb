-----------------------------------------------------------------------
--  util-encoders-base62 -- Encode/Decode a stream in Base62
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body Util.Encoders.Base62 is

   use Interfaces;

   --  ------------------------------
   --  Encodes the binary input stream represented by `Data` into
   --  the a base62 output stream `Into`.
   --
   --  If the transformer does not have enough room to write the result,
   --  it must return in `Encoded` the index of the last encoded
   --  position in the `Data` stream.
   --
   --  The transformer returns in `Last` the last valid position
   --  in the output stream `Into`.
   --
   --  The `Encoding_Error` exception is raised if the input
   --  stream cannot be transformed.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Encoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is
      --  Get 8 bytes as a 64-bit big-endian number and generate 11 bytes.
      Pos   : Ada.Streams.Stream_Element_Offset := Into'First;
      I     : Ada.Streams.Stream_Element_Offset := Data'First;
      Value : Unsigned_64 := E.Value;
      State : Encode_State_Type := E.State;
   begin
      while I <= Data'Last loop
         if State = Encode_State_Type'Last then
            --  The block is complete with this byte, make sure we can write it.
            exit when Pos + ENCODE_LENGTH - 1 > Into'Last;
            Value := Shift_Left (Value, 8) or Unsigned_64 (Data (I));
            for J in reverse Pos .. Pos + ENCODE_LENGTH - 1 loop
               Into (J) := BASE62_ALPHABET (Value mod 62);
               Value := Value / 62;
            end loop;
            Pos := Pos + ENCODE_LENGTH;
            State := 0;
         else
            Value := Shift_Left (Value, 8) or Unsigned_64 (Data (I));
            State := State + 1;
         end if;
         I := I + 1;
      end loop;

      E.State := State;
      E.Value := Value;
      Last    := Pos - 1;
      Encoded := I - 1;
   end Transform;

   --  ------------------------------
   --  Finish encoding the input array.
   --  ------------------------------
   overriding
   procedure Finish (E    : in out Encoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset) is
      Pos   : Ada.Streams.Stream_Element_Offset := Into'First;
      Value : Unsigned_64 := E.Value;
   begin
      --  Write the last block of 1 to 7 bytes.
      if E.State /= 0 then
         Pos := Pos + ENCODE_LENGTHS (E.State);
         for J in reverse Into'First .. Pos - 1 loop
            Into (J) := BASE62_ALPHABET (Value mod 62);
            Value := Value / 62;
         end loop;
      end if;

      --  Reset the state for a next encoding.
      E.State := 0;
      E.Value := 0;
      Last := Pos - 1;
   end Finish;

   --  ------------------------------
   --  Create a base62 encoder.
   --  ------------------------------
   function Create_Encoder return Transformer_Access is
   begin
      return new Encoder;
   end Create_Encoder;

   --  ------------------------------
   --  Decodes the base62 input stream represented by `Data` into
   --  the binary output stream `Into`.
   --
   --  If the transformer does not have enough room to write the result,
   --  it must return in `Encoded` the index of the last encoded
   --  position in the `Data` stream.
   --
   --  The transformer returns in `Last` the last valid position
   --  in the output stream `Into`.
   --
   --  The `Encoding_Error` exception is raised if the input
   --  stream cannot be transformed.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is
      --  Get 11 bytes as a 64-bit number in base 62 and generate 8 bytes.
      Pos   : Ada.Streams.Stream_Element_Offset := Into'First;
      I     : Ada.Streams.Stream_Element_Offset := Data'First;
      C     : Ada.Streams.Stream_Element;
      Val   : Unsigned_64;
      Value : Unsigned_64 := E.Value;
      State : Decode_State_Type := E.State;
   begin
      while I <= Data'Last loop
         C := Data (I);
         Val := Unsigned_64 (BASE62_VALUES (C));
         if Val > BASE62_ALPHABET'Last then
            raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
         end if;
         if State = Decode_State_Type'Last then
            --  The block is complete with this character, make sure we can write it.
            exit when Pos + BLOCK_LENGTH - 1 > Into'Last;
            if Value > (Unsigned_64'Last - Val) / 62 then
               raise Encoding_Error with "Invalid block: value is too large";
            end if;
            Value := Value * 62 + Val;
            for J in reverse Pos .. Pos + BLOCK_LENGTH - 1 loop
               Into (J) := Stream_Element (Value and 16#FF#);
               Value := Shift_Right (Value, 8);
            end loop;
            Pos := Pos + BLOCK_LENGTH;
            State := 0;
         else
            Value := Value * 62 + Val;
            State := State + 1;
         end if;
         I := I + 1;
      end loop;

      E.State := State;
      E.Value := Value;
      Last    := Pos - 1;
      Encoded := I - 1;

   exception
      when Encoding_Error =>
         --  Reset the state for a next decoding.
         E.State := 0;
         E.Value := 0;
         raise;
   end Transform;

   --  ------------------------------
   --  Finish decoding the input array.  The `Encoding_Error` exception is raised
   --  if the last block is not a valid base62 block.
   --  ------------------------------
   overriding
   procedure Finish (E    : in out Decoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset) is
      Pos   : Ada.Streams.Stream_Element_Offset := Into'First;
      Value : Unsigned_64 := E.Value;
      State : constant Decode_State_Type := E.State;
   begin
      --  Reset the state for a next decoding.
      E.State := 0;
      E.Value := 0;

      --  Write the last block of 1 to 7 bytes.
      if State /= 0 then
         if DECODE_LENGTHS (State) = 0 then
            raise Encoding_Error with "Invalid block: length is not valid";
         end if;
         if Shift_Right (Value, 8 * Natural (DECODE_LENGTHS (State))) /= 0 then
            raise Encoding_Error with "Invalid block: value is too large";
         end if;
         Pos := Pos + DECODE_LENGTHS (State);
         for J in reverse Into'First .. Pos - 1 loop
            Into (J) := Stream_Element (Value and 16#FF#);
            Value := Shift_Right (Value, 8);
         end loop;
      end if;
      Last := Pos - 1;
   end Finish;

   --  ------------------------------
   --  Create a base62 decoder.
   --  ------------------------------
   function Create_Decoder return Transformer_Access is
   begin
      return new Decoder;
   end Create_Decoder;

end Util.Encoders.Base62;
