-----------------------------------------------------------------------
--  util-encoders-base64 -- Encode/Decode a stream in Base64
--  Copyright (C) 2009, 2010, 2011, 2012, 2016, 2017 Stephane Carrez
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

--  The <b>Util.Encodes.Base64</b> packages encodes and decodes streams
--  in Base64 (See rfc4648: The Base16, Base32, and Base64 Data Encodings).
package Util.Encoders.Base64 is

   pragma Preelaborate;

   --  Encode the 64-bit value to LEB128 and then base64url.
   function Encode (Value : in Interfaces.Unsigned_64) return String;

   --  Decode the base64url string and then the LEB128 integer.
   --  Raise the Encoding_Error if the string is invalid and cannot be decoded.
   function Decode (Value : in String) return Interfaces.Unsigned_64;

   --  ------------------------------
   --  Base64 encoder
   --  ------------------------------
   --  This <b>Encoder</b> translates the (binary) input stream into
   --  a Base64 ascii stream.
   type Encoder is new Util.Encoders.Transformer with private;

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

   --  Create a base64 encoder using the URL alphabet.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   function Create_URL_Encoder return Transformer_Access;

   --  ------------------------------
   --  Base64 decoder
   --  ------------------------------
   --  The <b>Decoder</b> decodes a Base64 ascii stream into a binary stream.
   type Decoder is new Util.Encoders.Transformer with private;

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
   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset);

   --  Set the decoder to use the base64 URL alphabet when <b>Mode</b> is True.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   procedure Set_URL_Mode (E    : in out Decoder;
                           Mode : in Boolean);

   --  Create a base64 decoder using the URL alphabet.
   --  The URL alphabet uses the '-' and '_' instead of the '+' and '/' characters.
   function Create_URL_Decoder return Transformer_Access;

private

   type Alphabet is
     array (Interfaces.Unsigned_8 range 0 .. 63) of Ada.Streams.Stream_Element;

   type Alphabet_Access is not null access constant Alphabet;

   BASE64_ALPHABET : aliased constant Alphabet :=
     (Character'Pos ('A'), Character'Pos ('B'), Character'Pos ('C'), Character'Pos ('D'),
      Character'Pos ('E'), Character'Pos ('F'), Character'Pos ('G'), Character'Pos ('H'),
      Character'Pos ('I'), Character'Pos ('J'), Character'Pos ('K'), Character'Pos ('L'),
      Character'Pos ('M'), Character'Pos ('N'), Character'Pos ('O'), Character'Pos ('P'),
      Character'Pos ('Q'), Character'Pos ('R'), Character'Pos ('S'), Character'Pos ('T'),
      Character'Pos ('U'), Character'Pos ('V'), Character'Pos ('W'), Character'Pos ('X'),
      Character'Pos ('Y'), Character'Pos ('Z'), Character'Pos ('a'), Character'Pos ('b'),
      Character'Pos ('c'), Character'Pos ('d'), Character'Pos ('e'), Character'Pos ('f'),
      Character'Pos ('g'), Character'Pos ('h'), Character'Pos ('i'), Character'Pos ('j'),
      Character'Pos ('k'), Character'Pos ('l'), Character'Pos ('m'), Character'Pos ('n'),
      Character'Pos ('o'), Character'Pos ('p'), Character'Pos ('q'), Character'Pos ('r'),
      Character'Pos ('s'), Character'Pos ('t'), Character'Pos ('u'), Character'Pos ('v'),
      Character'Pos ('w'), Character'Pos ('x'), Character'Pos ('y'), Character'Pos ('z'),
      Character'Pos ('0'), Character'Pos ('1'), Character'Pos ('2'), Character'Pos ('3'),
      Character'Pos ('4'), Character'Pos ('5'), Character'Pos ('6'), Character'Pos ('7'),
      Character'Pos ('8'), Character'Pos ('9'), Character'Pos ('+'), Character'Pos ('/'));

   BASE64_URL_ALPHABET : aliased constant Alphabet :=
     (Character'Pos ('A'), Character'Pos ('B'), Character'Pos ('C'), Character'Pos ('D'),
      Character'Pos ('E'), Character'Pos ('F'), Character'Pos ('G'), Character'Pos ('H'),
      Character'Pos ('I'), Character'Pos ('J'), Character'Pos ('K'), Character'Pos ('L'),
      Character'Pos ('M'), Character'Pos ('N'), Character'Pos ('O'), Character'Pos ('P'),
      Character'Pos ('Q'), Character'Pos ('R'), Character'Pos ('S'), Character'Pos ('T'),
      Character'Pos ('U'), Character'Pos ('V'), Character'Pos ('W'), Character'Pos ('X'),
      Character'Pos ('Y'), Character'Pos ('Z'), Character'Pos ('a'), Character'Pos ('b'),
      Character'Pos ('c'), Character'Pos ('d'), Character'Pos ('e'), Character'Pos ('f'),
      Character'Pos ('g'), Character'Pos ('h'), Character'Pos ('i'), Character'Pos ('j'),
      Character'Pos ('k'), Character'Pos ('l'), Character'Pos ('m'), Character'Pos ('n'),
      Character'Pos ('o'), Character'Pos ('p'), Character'Pos ('q'), Character'Pos ('r'),
      Character'Pos ('s'), Character'Pos ('t'), Character'Pos ('u'), Character'Pos ('v'),
      Character'Pos ('w'), Character'Pos ('x'), Character'Pos ('y'), Character'Pos ('z'),
      Character'Pos ('0'), Character'Pos ('1'), Character'Pos ('2'), Character'Pos ('3'),
      Character'Pos ('4'), Character'Pos ('5'), Character'Pos ('6'), Character'Pos ('7'),
      Character'Pos ('8'), Character'Pos ('9'), Character'Pos ('-'), Character'Pos ('_'));

   type Encoder is new Util.Encoders.Transformer with record
      Alphabet : Alphabet_Access := BASE64_ALPHABET'Access;
      Column   : Natural := 0;
      Count    : Natural := 0;
      Value    : Interfaces.Unsigned_8;
   end record;

   type Alphabet_Values is array (Ada.Streams.Stream_Element) of Interfaces.Unsigned_8;
   type Alphabet_Values_Access is not null access constant Alphabet_Values;

   BASE64_VALUES : aliased constant Alphabet_Values :=
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
      Character'Pos ('a') => 26, Character'Pos ('b') => 27,
      Character'Pos ('c') => 28, Character'Pos ('d') => 29,
      Character'Pos ('e') => 30, Character'Pos ('f') => 31,
      Character'Pos ('g') => 32, Character'Pos ('h') => 33,
      Character'Pos ('i') => 34, Character'Pos ('j') => 35,
      Character'Pos ('k') => 36, Character'Pos ('l') => 37,
      Character'Pos ('m') => 38, Character'Pos ('n') => 39,
      Character'Pos ('o') => 40, Character'Pos ('p') => 41,
      Character'Pos ('q') => 42, Character'Pos ('r') => 43,
      Character'Pos ('s') => 44, Character'Pos ('t') => 45,
      Character'Pos ('u') => 46, Character'Pos ('v') => 47,
      Character'Pos ('w') => 48, Character'Pos ('x') => 49,
      Character'Pos ('y') => 50, Character'Pos ('z') => 51,
      Character'Pos ('0') => 52, Character'Pos ('1') => 53,
      Character'Pos ('2') => 54, Character'Pos ('3') => 55,
      Character'Pos ('4') => 56, Character'Pos ('5') => 57,
      Character'Pos ('6') => 58, Character'Pos ('7') => 59,
      Character'Pos ('8') => 60, Character'Pos ('9') => 61,
      Character'Pos ('+') => 62, Character'Pos ('/') => 63,
     others => 16#FF#);

   BASE64_URL_VALUES : aliased constant Alphabet_Values :=
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
      Character'Pos ('a') => 26, Character'Pos ('b') => 27,
      Character'Pos ('c') => 28, Character'Pos ('d') => 29,
      Character'Pos ('e') => 30, Character'Pos ('f') => 31,
      Character'Pos ('g') => 32, Character'Pos ('h') => 33,
      Character'Pos ('i') => 34, Character'Pos ('j') => 35,
      Character'Pos ('k') => 36, Character'Pos ('l') => 37,
      Character'Pos ('m') => 38, Character'Pos ('n') => 39,
      Character'Pos ('o') => 40, Character'Pos ('p') => 41,
      Character'Pos ('q') => 42, Character'Pos ('r') => 43,
      Character'Pos ('s') => 44, Character'Pos ('t') => 45,
      Character'Pos ('u') => 46, Character'Pos ('v') => 47,
      Character'Pos ('w') => 48, Character'Pos ('x') => 49,
      Character'Pos ('y') => 50, Character'Pos ('z') => 51,
      Character'Pos ('0') => 52, Character'Pos ('1') => 53,
      Character'Pos ('2') => 54, Character'Pos ('3') => 55,
      Character'Pos ('4') => 56, Character'Pos ('5') => 57,
      Character'Pos ('6') => 58, Character'Pos ('7') => 59,
      Character'Pos ('8') => 60, Character'Pos ('9') => 61,
      Character'Pos ('-') => 62, Character'Pos ('_') => 63,
      others => 16#FF#);

   type Decoder is new Util.Encoders.Transformer with record
      Values : Alphabet_Values_Access := BASE64_VALUES'Access;
   end record;

end Util.Encoders.Base64;
