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
with Ada.Streams;
with Interfaces;

--  The `Util.Encodes.Base32` packages encodes and decodes streams
--  in Base32 (See rfc4648: The Base16, Base32, and Base64 Data Encodings).
package Util.Encoders.Base32 is

   pragma Preelaborate;

   --  ------------------------------
   --  Base32 encoder
   --  ------------------------------
   --  This `Encoder` translates the (binary) input stream into
   --  a Base32 ascii stream.
   type Encoder is new Util.Encoders.Transformer with private;

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
   overriding
   procedure Transform (E       : in out Encoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset);

   --  Finish encoding the input array.
   overriding
   procedure Finish (E    : in out Encoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset);

   --  Set the encoder to use the base64 URL alphabet when <b>Mode</b> is True.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   procedure Set_URL_Mode (E    : in out Encoder;
                           Mode : in Boolean);

   --  Create a base32 encoder using the default Base32 alphabet.
   function Create_Encoder return Transformer_Access;

   --  ------------------------------
   --  Base32 decoder
   --  ------------------------------
   --  The `Decoder` decodes a Base32 ascii stream into a binary stream.
   type Decoder is new Util.Encoders.Transformer with private;

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
   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset);

   --  Finish decoder the input array.
   overriding
   procedure Finish (E    : in out Decoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset);

   --  Set the decoder to use the base32 URL alphabet when <b>Mode</b> is True.
   procedure Set_URL_Mode (E    : in out Decoder;
                           Mode : in Boolean);

   --  Create a base32 decoder using the Base32 alphabet.
   function Create_Decoder return Transformer_Access;

private

   type Alphabet is
     array (Interfaces.Unsigned_8 range 0 .. 31) of Ada.Streams.Stream_Element;

   type Alphabet_Access is not null access constant Alphabet;

   BASE32_ALPHABET : aliased constant Alphabet :=
     (Character'Pos ('A'), Character'Pos ('B'), Character'Pos ('C'), Character'Pos ('D'),
      Character'Pos ('E'), Character'Pos ('F'), Character'Pos ('G'), Character'Pos ('H'),
      Character'Pos ('I'), Character'Pos ('J'), Character'Pos ('K'), Character'Pos ('L'),
      Character'Pos ('M'), Character'Pos ('N'), Character'Pos ('O'), Character'Pos ('P'),
      Character'Pos ('Q'), Character'Pos ('R'), Character'Pos ('S'), Character'Pos ('T'),
      Character'Pos ('U'), Character'Pos ('V'), Character'Pos ('W'), Character'Pos ('X'),
      Character'Pos ('Y'), Character'Pos ('Z'), Character'Pos ('2'), Character'Pos ('3'),
      Character'Pos ('4'), Character'Pos ('5'), Character'Pos ('6'), Character'Pos ('7'));

   type Encode_State_Type is new Natural range 0 .. 7;

   type Encoder is new Util.Encoders.Transformer with record
      Alphabet : Alphabet_Access := BASE32_ALPHABET'Access;
      Column   : Natural := 0;
      Remain   : Interfaces.Unsigned_8;
      State    : Encode_State_Type := 0;
   end record;

   type Alphabet_Values is array (Ada.Streams.Stream_Element) of Interfaces.Unsigned_8;
   type Alphabet_Values_Access is not null access constant Alphabet_Values;

   BASE32_VALUES : aliased constant Alphabet_Values :=
     (Character'Pos ('A') => 0,  Character'Pos ('B') => 1,
      Character'Pos ('C') => 2,  Character'Pos ('D') => 3,
      Character'Pos ('E') => 4,  Character'Pos ('F') => 5,
      Character'Pos ('G') => 6,  Character'Pos ('H') => 7,
      Character'Pos ('I') => 8,  Character'Pos ('J') => 9,
      Character'Pos ('K') => 10, Character'Pos ('L') => 11,
      Character'Pos ('M') => 12, Character'Pos ('N') => 13,
      Character'Pos ('O') => 14, Character'Pos ('P') => 15,
      Character'Pos ('Q') => 16, Character'Pos ('R') => 17,
      Character'Pos ('S') => 18, Character'Pos ('T') => 19,
      Character'Pos ('U') => 20, Character'Pos ('V') => 21,
      Character'Pos ('W') => 22, Character'Pos ('X') => 23,
      Character'Pos ('Y') => 24, Character'Pos ('Z') => 25,
      Character'Pos ('2') => 26, Character'Pos ('3') => 27,
      Character'Pos ('4') => 28, Character'Pos ('5') => 29,
      Character'Pos ('6') => 30, Character'Pos ('7') => 31,
     others => 16#FF#);

   type Decode_State_Type is new Natural range 0 .. 7 + 6;

   type Decoder is new Util.Encoders.Transformer with record
      Values   : Alphabet_Values_Access := BASE32_VALUES'Access;
      Remain   : Interfaces.Unsigned_8;
      State    : Decode_State_Type := 0;
   end record;

end Util.Encoders.Base32;
