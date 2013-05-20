-----------------------------------------------------------------------
--  util-encoders-base64 -- Encode/Decode a stream in base64adecimal
--  Copyright (C) 2009, 2010, 2011, 2012, 2013 Stephane Carrez
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

package body Util.Encoders.Base64 is

   use Interfaces;
   use Ada;

   use type Ada.Streams.Stream_Element_Offset;

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
   procedure Transform (E       : in Encoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is

      Pos      : Streams.Stream_Element_Offset := Into'First;
      I        : Streams.Stream_Element_Offset := Data'First;
      C1, C2   : Unsigned_8;

      Alphabet : constant Alphabet_Access := E.Alphabet;

   begin
      while I <= Data'Last loop
         if Pos + 4 > Into'Last + 1 then
            Last    := Pos - 1;
            Encoded := I - 1;
            return;
         end if;

         --  Encode the first byte, add padding if necessary.
         C1 := Unsigned_8 (Data (I));
         Into (Pos) := Alphabet (Shift_Right (C1, 2));
         if I = Data'Last then
            Into (Pos + 1) := Alphabet (Shift_Left (C1 and 3, 4));
            Into (Pos + 2) := Character'Pos ('=');
            Into (Pos + 3) := Character'Pos ('=');
            Last := Pos + 3;
            Encoded := Data'Last;
            return;
         end if;

         --  Encode the second byte, add padding if necessary.
         C2 := Unsigned_8 (Data (I + 1));
         Into (Pos + 1) := Alphabet (Shift_Left (C1 and 16#03#, 4) or Shift_Right (C2, 4));
         if I = Data'Last - 1 then
            Into (Pos + 2) := Alphabet (Shift_Left (C2 and 16#0F#, 2));
            Into (Pos + 3) := Character'Pos ('=');
            Last := Pos + 3;
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
      Last    := Pos - 1;
      Encoded := Data'Last;
   end Transform;

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
      return new Encoder '(Alphabet => BASE64_URL_ALPHABET'Access);
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
   procedure Transform (E       : in Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is

      use Ada.Streams;

      Pos      : Streams.Stream_Element_Offset := Into'First;
      I        : Streams.Stream_Element_Offset := Data'First;
      C1, C2   : Streams.Stream_Element;
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
