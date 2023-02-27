-----------------------------------------------------------------------
--  util-encoders-base32 -- Encode/Decode a stream in Base32
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
package body Util.Encoders.Base32 is

   use Interfaces;

   --  ------------------------------
   --  Encodes the binary input stream represented by `Data` into
   --  the a base32 output stream `Into`.
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
      --  Get 5 bytes generates 8 bytes
      --                    C                    C-1
      --  0: { 5.0 }        0x1F  rshift 3       0x0
      --  1: { 3.0 + 2.1}   0x03  rshift 6       0x1C  lshift 2
      --  2: { 5.1 }                             0x1F  lshift 1
      --  3: { 1.1 + 4.2}   0x0F  lshift 4       0x10  rshift 4
      --  4: { 4.2 + 1.3}   0x01  lshift 7       0x1E  rshift 1
      --  5: { 5.3 }                             0x1F  lshift 2
      --  6: { 2.3 + 3.4}   0x07  lshift 5       0x18  rshift 3
      --  7: { 5.4 }                             0x1F  rshift 0
      Pos      : Ada.Streams.Stream_Element_Offset := Into'First;
      I        : Ada.Streams.Stream_Element_Offset := Data'First;
      C        : Unsigned_8;
      Alphabet : constant Alphabet_Access := E.Alphabet;
      R        : Unsigned_8 := E.Remain;
      State    : Encode_State_Type := E.State;
   begin
      while Pos <= Into'Last loop
         case State is
            when 0 =>
               exit when I > Data'Last;
               State := 1;
               C := Unsigned_8 (Data (I));
               Into (Pos) := Alphabet (Shift_Right (C, 3));
               R := Shift_Left (C, 2) and 16#1C#;
               Pos := Pos + 1;
               I := I + 1;

            when 1 =>
               exit when I > Data'Last;
               State := 2;
               C := Unsigned_8 (Data (I));
               Into (Pos) := Alphabet (Shift_Right (C and 16#C0#, 6) or R);
               R := C;
               Pos := Pos + 1;
               I := I + 1;

            when 2 =>
               State := 3;
               Into (Pos) := Alphabet (Shift_Right (R and 16#3E#, 1));
               R := Shift_Left (R, 4) and 16#10#;
               Pos := Pos + 1;

            when 3 =>
               exit when I > Data'Last;
               State := 4;
               C := Unsigned_8 (Data (I));
               Into (Pos) := Alphabet (Shift_Right (C and 16#F0#, 4) or R);
               R := Shift_Left (C, 1) and 16#1E#;
               Pos := Pos + 1;
               I := I + 1;

            when 4 =>
               exit when I > Data'Last;
               State := 5;
               C := Unsigned_8 (Data (I));
               Into (Pos) := Alphabet (Shift_Right (C and 16#80#, 7) or R);
               R := C;
               Pos := Pos + 1;
               I := I + 1;

            when 5 =>
               State := 6;
               Into (Pos) := Alphabet (Shift_Right (R and 16#7C#, 2));
               R := Shift_Left (R, 3) and 16#18#;
               Pos := Pos + 1;

            when 6 =>
               exit when I > Data'Last;
               State := 7;
               C := Unsigned_8 (Data (I));
               Into (Pos) := Alphabet (Shift_Right (C and 16#E0#, 5) or R);
               R := C and 16#1F#;
               Pos := Pos + 1;
               I := I + 1;

            when 7 =>
               State := 0;
               Into (Pos) := Alphabet (R);
               Pos := Pos + 1;

         end case;
      end loop;

      E.State := State;
      E.Remain := R;
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
      Pos      : Ada.Streams.Stream_Element_Offset := Into'First;
      Alphabet : constant Alphabet_Access := E.Alphabet;
      R        : Unsigned_8 := E.Remain;
   begin
      --          +---+--------+---+-------+-------+---+------+----+
      --  Output: | 0 |   1    | 2 |   3   |    4  | 5 |   6  | 7  |
      --          +---+----+---+---+---+---+---+---+---+---+--+----+
      --  Input:  | 5 + 3  | 2 + 5 + 1 | 4 + 4 | 1 + 5 + 2 | 3 + 5 |
      --          +--------+-----------+-------+-----------+-------+
      while E.State /= 0 loop
         case E.State is
            when 1 =>
               E.State := 0;
               Into (Pos) := Alphabet (R);
               Into (Pos + 1 .. Pos + 7) := (others => Character'Pos ('='));
               Pos := Pos + 7;

            when 2 =>
               E.State := 3;
               Into (Pos) := Alphabet (Shift_Right (R and 16#3E#, 1));
               R := Shift_Left (R, 4) and 16#10#;
               Pos := Pos + 1;

            when 3 =>
               E.State := 0;
               Into (Pos) := Alphabet (R);
               Into (Pos + 1 .. Pos + 5) := (others => Character'Pos ('='));
               Pos := Pos + 5;

            when 4 =>
               E.State := 0;
               Into (Pos) := Alphabet (R);
               Into (Pos + 1 .. Pos + 4) := (others => Character'Pos ('='));
               Pos := Pos + 4;

            when 5 =>
               E.State := 6;
               Into (Pos) := Alphabet (Shift_Right (R and 16#7C#, 2));
               R := Shift_Left (R, 3) and 16#18#;
               Pos := Pos + 1;

            when 6 =>
               E.State := 0;
               Into (Pos) := Alphabet (R);
               Into (Pos + 1) := Character'Pos ('=');
               Pos := Pos + 2;

            when 7 =>
               E.State := 0;
               Into (Pos) := Alphabet (R);
               Pos := Pos + 1;

            when others =>
               null;

         end case;
      end loop;
      Last := Pos - 1;
   end Finish;

   --  ------------------------------
   --  Set the encoder to use the base64 URL alphabet when <b>Mode</b> is True.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   --  ------------------------------
   procedure Set_URL_Mode (E    : in out Encoder;
                           Mode : in Boolean) is
   begin
      if Mode then
         E.Alphabet := BASE32_ALPHABET'Access;
      else
         E.Alphabet := BASE32_ALPHABET'Access;
      end if;
   end Set_URL_Mode;

   --  ------------------------------
   --  Create a base32 encoder using the URL alphabet.
   --  ------------------------------
   function Create_Encoder return Transformer_Access is
   begin
      return new Encoder '(Alphabet => BASE32_ALPHABET'Access, others => <>);
   end Create_Encoder;

   --  ------------------------------
   --  Decodes the base32 input stream represented by `Data` into
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
      Pos      : Ada.Streams.Stream_Element_Offset := Into'First;
      I        : Ada.Streams.Stream_Element_Offset := Data'First;
      C        : Ada.Streams.Stream_Element;
      Val      : Unsigned_8;

      Values   : constant Alphabet_Values_Access := E.Values;
      R        : Unsigned_8 := E.Remain;
      State    : Decode_State_Type := E.State;

      --  Get 8 bytes and generate 5 bytes:
      --          +---+--------+---+-------+-------+---+------+----+
      --  Input:  | 0 |   1    | 2 |   3   |    4  | 5 |   6  | 7  |
      --          +---+----+---+---+---+---+---+---+---+---+--+----+
      --  Output: | 5 + 3  | 2 + 5 + 1 | 4 + 4 | 1 + 5 + 2 | 3 + 5 |
      --          +--------+-----------+-------+-----------+-------+
   begin
      while Pos <= Into'Last loop
         case State is
            when 0 =>
               exit when I > Data'Last;
               State := 1;
               C := Data (I);
               Val := Values (C);
               if (Val and 16#E0#) /= 0 then
                  raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
               end if;
               R := Shift_Left (Val, 3);
               I := I + 1;

            when 1 =>
               exit when I > Data'Last;
               State := 2;
               C := Data (I);
               Val := Values (C);
               if (Val and 16#E0#) /= 0 then
                  raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
               end if;
               Into (Pos) := Stream_Element (R or Shift_Right (Val and 16#1C#, 2));
               R := Shift_Left (Val, 6);
               Pos := Pos + 1;
               I := I + 1;

            when 2 =>
               exit when I > Data'Last;
               State := 3;
               C := Data (I);
               Val := Values (C);
               if (Val and 16#E0#) /= 0 then
                  if C /= Character'Pos ('=') then
                     raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
                  end if;
                  State := Decode_State_Type'Last - 5;  --  Expect 5 more '='.
                  Val := 0;
               end if;
               R := R or Shift_Left (Val, 1);
               I := I + 1;

            when 3 =>
               exit when I > Data'Last;
               State := 4;
               C := Data (I);
               Val := Values (C);
               if (Val and 16#E0#) /= 0 then
                  if C /= Character'Pos ('=') then
                     raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
                  end if;
                  State := Decode_State_Type'Last - 4;  --  Expect 4 more '='.
                  Val := 0;
               end if;
               Into (Pos) := Stream_Element (R or Shift_Right (Val, 4));
               R := Shift_Left (Val, 4);
               Pos := Pos + 1;
               I := I + 1;

            when 4 =>
               exit when I > Data'Last;
               State := 5;
               C := Data (I);
               Val := Values (C);
               if (Val and 16#E0#) /= 0 then
                  if C /= Character'Pos ('=') then
                     raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
                  end if;
                  State := Decode_State_Type'Last - 3;  --  Expect 3 more '='.
               else
                  Into (Pos) := Stream_Element (R or Shift_Right (Val, 1));
                  R := Shift_Left (Val, 7);
                  Pos := Pos + 1;
               end if;
               I := I + 1;

            when 5 =>
               exit when I > Data'Last;
               State := 6;
               C := Data (I);
               Val := Values (C);
               if (Val and 16#E0#) /= 0 then
                  if C /= Character'Pos ('=') then
                     raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
                  end if;
                  State := Decode_State_Type'Last - 2;  --  Expect 2 more '='.
               else
                  R := R or Shift_Left (Val, 2);
               end if;
               I := I + 1;

            when 6 =>
               exit when I > Data'Last;
               State := 7;
               C := Data (I);
               Val := Values (C);
               if (Val and 16#E0#) /= 0 then
                  if C /= Character'Pos ('=') then
                     raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
                  end if;
                  State := Decode_State_Type'Last - 2;  --  Expect 2 more '='.
               else
                  Into (Pos) := Stream_Element (R or Shift_Right (Val, 3));
                  R := Shift_Left (Val, 5);
                  Pos := Pos + 1;
               end if;
               I := I + 1;

            when 7 =>
               exit when I > Data'Last;
               State := 0;
               C := Data (I);
               Val := Values (C);
               if (Val and 16#E0#) /= 0 then
                  if C /= Character'Pos ('=') then
                     raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
                  end if;
                  Val := 0;
                  State := Decode_State_Type'Last;  --  Expect no more '='.
               else
                  Into (Pos) := Stream_Element (R or Val);
                  Pos := Pos + 1;
               end if;
               I := I + 1;

            when others =>
               exit when I > Data'Last;
               if State = Decode_State_Type'Last then
                  raise Encoding_Error with "Garbage after last valid '='";
               end if;
               C := Data (I);
               if C /= Character'Pos ('=') then
                  raise Encoding_Error with "Invalid character '" & Character'Val (C) & "'";
               end if;
               State := State + 1;
               I := I + 1;

         end case;
      end loop;

      E.State := State;
      E.Remain := R;
      Last    := Pos - 1;
      Encoded := I - 1;
   end Transform;

   --  ------------------------------
   --  Finish decoder the input array.
   --  ------------------------------
   overriding
   procedure Finish (E    : in out Decoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset) is
   begin
      --  Reset the state for a next decoding.
      E.State := 0;
      E.Remain := 0;
      Last := Into'First - 1;
   end Finish;

   --  ------------------------------
   --  Set the decoder to use the base64 URL alphabet when <b>Mode</b> is True.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   --  ------------------------------
   procedure Set_URL_Mode (E    : in out Decoder;
                           Mode : in Boolean) is
   begin
      if Mode then
         E.Values := BASE32_VALUES'Access;
      else
         E.Values := BASE32_VALUES'Access;
      end if;
   end Set_URL_Mode;

   --  ------------------------------
   --  Create a base64 decoder using the URL alphabet.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   --  ------------------------------
   function Create_Decoder return Transformer_Access is
   begin
      return new Decoder '(Values => BASE32_VALUES'Access, others => <>);
   end Create_Decoder;

end Util.Encoders.Base32;
