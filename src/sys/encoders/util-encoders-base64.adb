-----------------------------------------------------------------------
--  util-encoders-base64 -- Encode/Decode a stream in base64
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2016, 2017, 2019, 2022 Stephane Carrez
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
with Util.Streams;
package body Util.Encoders.Base64 is

   use Interfaces;

   --  ------------------------------
   --  Encode the 64-bit value to LEB128 and then base64url.
   --  ------------------------------
   function Encode (Value : in Interfaces.Unsigned_64) return String is
      E       : Encoder;
      Data    : Ada.Streams.Stream_Element_Array (1 .. 10);
      Last    : Ada.Streams.Stream_Element_Offset;
      Encoded : Ada.Streams.Stream_Element_Offset;
      Result  : String (1 .. 16);
      Buf     : Ada.Streams.Stream_Element_Array (1 .. Result'Length);
      for Buf'Address use Result'Address;
      pragma Import (Ada, Buf);
   begin
      --  Encode the integer to LEB128 in the data buffer.
      Encode_LEB128 (Into => Data,
                     Pos  => Data'First,
                     Val  => Value,
                     Last => Last);

      --  Encode the data buffer in base64url.
      E.Set_URL_Mode (True);
      E.Transform (Data    => Data (Data'First .. Last),
                   Into    => Buf,
                   Last    => Last,
                   Encoded => Encoded);
      E.Finish (Into => Buf (Last + 1 .. Buf'Last),
                Last => Last);

      --  Strip the '=' or '==' at end of base64url.
      if Result (Positive (Last)) /= '=' then
         return Result (Result'First .. Positive (Last));
      elsif Result (Positive (Last) - 1) /= '=' then
         return Result (Result'First .. Positive (Last) - 1);
      else
         return Result (Result'First .. Positive (Last) - 2);
      end if;
   end Encode;

   --  ------------------------------
   --  Decode the base64url string and then the LEB128 integer.
   --  Raise the Encoding_Error if the string is invalid and cannot be decoded.
   --  ------------------------------
   function Decode (Value : in String) return Interfaces.Unsigned_64 is
      D       : Decoder;
      Buf     : Ada.Streams.Stream_Element_Array (1 .. Value'Length + 2);
      R       : Ada.Streams.Stream_Element_Array (1 .. Value'Length + 2);
      Last    : Ada.Streams.Stream_Element_Offset;
      Encoded : Ada.Streams.Stream_Element_Offset;
      Result  : Interfaces.Unsigned_64;
      End_Pos : constant Ada.Streams.Stream_Element_Offset
        := Value'Length + ((4 - (Value'Length mod 4)) mod 4);
   begin
      if Buf'Length < End_Pos then
         raise Encoding_Error with "Input string is too short";
      end if;
      Util.Streams.Copy (Into => Buf (1 .. Value'Length), From => Value);

      --  Set back the '=' for the base64url (pad to multiple of 4.
      Buf (Value'Length + 1) := Character'Pos ('=');
      Buf (Value'Length + 2) := Character'Pos ('=');

      --  Decode using base64url
      D.Set_URL_Mode (True);
      D.Transform (Data    => Buf (Buf'First .. End_Pos),
                   Into    => R,
                   Last    => Last,
                   Encoded => Encoded);
      if Encoded /= End_Pos then
         raise Encoding_Error with "Input string is too short";
      end if;

      --  Decode the LEB128 number.
      Decode_LEB128 (From => R (R'First .. Last),
                     Pos  => R'First,
                     Val  => Result,
                     Last => Encoded);

      --  Check that everything was decoded.
      if Last + 1 /= Encoded then
         raise Encoding_Error with "Input string contains garbage at the end";
      end if;
      return Result;
   end Decode;

   --  ------------------------------
   --  Encodes the binary input stream represented by <b>Data</b> into
   --  the a base64 output stream <b>Into</b>.
   --
   --  If the transformer does not have enough room to write the result,
   --  it must return in <b>Encoded</b> the index of the last encoded
   --  position in the <b>Data</b> stream.
   --
   --  The transformer returns in <b>Last</b> the last valid position
   --  in the output stream <b>Into</b>.
   --
   --  The <b>Encoding_Error</b> exception is raised if the input
   --  stream cannot be transformed.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Encoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is

      Pos      : Ada.Streams.Stream_Element_Offset := Into'First;
      I        : Ada.Streams.Stream_Element_Offset := Data'First;
      C1, C2   : Unsigned_8;

      Alphabet : constant Alphabet_Access := E.Alphabet;

   begin
      while E.Count /= 0 and then I <= Data'Last loop
         if E.Count = 2 then
            C1 := E.Value;
            C2 := Unsigned_8 (Data (I));
            Into (Pos) := Alphabet (Shift_Left (C1 and 16#03#, 4) or Shift_Right (C2, 4));
            Pos := Pos + 1;
            I := I + 1;
            E.Count := 1;
            E.Value := C2;
         else
            C2 := E.Value;
            C1 := Unsigned_8 (Data (I));
            Into (Pos) := Alphabet (Shift_Left (C2 and 16#0F#, 2) or Shift_Right (C1, 6));
            Into (Pos + 1) := Alphabet (C1 and 16#03F#);
            Pos := Pos + 2;
            I := I + 1;
            E.Count := 0;
         end if;
      end loop;
      while I <= Data'Last loop
         if Pos + 4 > Into'Last + 1 then
            Last    := Pos - 1;
            Encoded := I - 1;
            E.Count := 0;
            return;
         end if;

         --  Encode the first byte, add padding if necessary.
         C1 := Unsigned_8 (Data (I));
         Into (Pos) := Alphabet (Shift_Right (C1, 2));
         if I = Data'Last then
            E.Value := C1;
            E.Count := 2;
            Last := Pos;
            Encoded := Data'Last;
            return;
         end if;

         --  Encode the second byte, add padding if necessary.
         C2 := Unsigned_8 (Data (I + 1));
         Into (Pos + 1) := Alphabet (Shift_Left (C1 and 16#03#, 4) or Shift_Right (C2, 4));
         if I = Data'Last - 1 then
            E.Value := C2;
            E.Count := 1;
            Last := Pos + 1;
            Encoded := Data'Last;
            return;
         end if;

         --  Encode the third byte
         C1 := Unsigned_8 (Data (I + 2));
         Into (Pos + 2) := Alphabet (Shift_Left (C2 and 16#0F#, 2) or Shift_Right (C1, 6));
         Into (Pos + 3) := Alphabet (C1 and 16#03F#);
         Pos := Pos + 4;
         I   := I + 3;
      end loop;
      E.Count := 0;
      Last    := Pos - 1;
      Encoded := Data'Last;
   end Transform;

   --  ------------------------------
   --  Finish encoding the input array.
   --  ------------------------------
   overriding
   procedure Finish (E    : in out Encoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset) is
      Pos : constant Ada.Streams.Stream_Element_Offset := Into'First;
   begin
      if E.Count = 2 then
         Into (Pos) := E.Alphabet (Shift_Left (E.Value and 3, 4));
         Into (Pos + 1) := Character'Pos ('=');
         Into (Pos + 2) := Character'Pos ('=');
         Last := Pos + 2;
         E.Count := 0;
      elsif E.Count = 1 then
         Into (Pos) := E.Alphabet (Shift_Left (E.Value and 16#0F#, 2));
         Into (Pos + 1) := Character'Pos ('=');
         Last := Pos + 1;
         E.Count := 0;
      else
         Last := Pos - 1;
      end if;
   end Finish;

   --  ------------------------------
   --  Set the encoder to use the base64 URL alphabet when <b>Mode</b> is True.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   --  ------------------------------
   procedure Set_URL_Mode (E    : in out Encoder;
                           Mode : in Boolean) is
   begin
      if Mode then
         E.Alphabet := BASE64_URL_ALPHABET'Access;
      else
         E.Alphabet := BASE64_ALPHABET'Access;
      end if;
   end Set_URL_Mode;

   --  ------------------------------
   --  Create a base64 encoder using the URL alphabet.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   --  ------------------------------
   function Create_URL_Encoder return Transformer_Access is
   begin
      return new Encoder '(Alphabet => BASE64_URL_ALPHABET'Access, others => <>);
   end Create_URL_Encoder;

   --  ------------------------------
   --  Decodes the base64 input stream represented by <b>Data</b> into
   --  the binary output stream <b>Into</b>.
   --
   --  If the transformer does not have enough room to write the result,
   --  it must return in <b>Encoded</b> the index of the last encoded
   --  position in the <b>Data</b> stream.
   --
   --  The transformer returns in <b>Last</b> the last valid position
   --  in the output stream <b>Into</b>.
   --
   --  The <b>Encoding_Error</b> exception is raised if the input
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
      C1, C2   : Ada.Streams.Stream_Element;
      Val1, Val2 : Unsigned_8;

      Values : constant Alphabet_Values_Access := E.Values;

   begin
      while I <= Data'Last loop
         if Pos + 3 > Into'Last + 1 then
            Last    := Pos - 1;
            Encoded := I - 1;
            return;
         end if;

         --  Decode the first two bytes to produce the first output byte
         C1 := Data (I);
         Val1 := Values (C1);
         if (Val1 and 16#C0#) /= 0 then
            raise Encoding_Error with "Invalid character '" & Character'Val (C1) & "'";
         end if;

         C2 := Data (I + 1);
         Val2 := Values (C2);
         if (Val2 and 16#C0#) /= 0 then
            raise Encoding_Error with "Invalid character '" & Character'Val (C2) & "'";
         end if;

         Into (Pos) := Stream_Element (Shift_Left (Val1, 2) or Shift_Right (Val2, 4));

         if I + 2 > Data'Last then
            Encoded := I + 1;
            Last    := Pos;
            return;
         end if;

         --  Decode the next byte
         C1 := Data (I + 2);
         Val1 := Values (C1);
         if (Val1 and 16#C0#) /= 0 then
            if C1 /= Character'Pos ('=') then
               raise Encoding_Error with "Invalid character '" & Character'Val (C1) & "'";
            end if;
            Encoded := I + 3;
            Last    := Pos;
            return;
         end if;

         Into (Pos + 1) := Stream_Element (Shift_Left (Val2, 4) or Shift_Right (Val1, 2));

         if I + 3 > Data'Last then
            Encoded := I + 2;
            Last    := Pos + 1;
            return;
         end if;

         C2 := Data (I + 3);
         Val2 := Values (C2);
         if (Val2 and 16#C0#) /= 0 then
            if C2 /= Character'Pos ('=') then
               raise Encoding_Error with "Invalid character '" & Character'Val (C2) & "'";
            end if;
            Encoded := I + 3;
            Last    := Pos + 1;
            return;
         end if;

         Into (Pos + 2) := Stream_Element (Shift_Left (Val1, 6) or Val2);
         Pos := Pos + 3;
         I   := I + 4;
      end loop;
      Last    := Pos - 1;
      Encoded := Data'Last;
   end Transform;

   --  ------------------------------
   --  Set the decoder to use the base64 URL alphabet when <b>Mode</b> is True.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   --  ------------------------------
   procedure Set_URL_Mode (E    : in out Decoder;
                           Mode : in Boolean) is
   begin
      if Mode then
         E.Values := BASE64_URL_VALUES'Access;
      else
         E.Values := BASE64_VALUES'Access;
      end if;
   end Set_URL_Mode;

   --  ------------------------------
   --  Create a base64 decoder using the URL alphabet.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   --  ------------------------------
   function Create_URL_Decoder return Transformer_Access is
   begin
      return new Decoder '(Values => BASE64_URL_VALUES'Access);
   end Create_URL_Decoder;

end Util.Encoders.Base64;
