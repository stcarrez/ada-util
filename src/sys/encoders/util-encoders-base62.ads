-----------------------------------------------------------------------
--  util-encoders-base62 -- Encode/Decode a stream in Base62
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams;
with Interfaces;

--  The `Util.Encoders.Base62` packages encodes and decodes streams
--  in Base62 by using the alphabet `0-9`, `A-Z` and `a-z`.
--
--  The binary stream is split in blocks of 8 bytes and each block is
--  taken as a 64-bit big-endian number which is written in base 62 by
--  using 11 characters.  The last block can be shorter and it is written
--  by using the smallest number of characters that can hold it: 2, 3, 5,
--  6, 7, 9 and 10 characters for a block of 1, 2, 3, 4, 5, 6 and 7 bytes.
--  There is no padding since the length of the last block identifies
--  the number of bytes.
package Util.Encoders.Base62 is

   pragma Preelaborate;

   --  ------------------------------
   --  Base62 encoder
   --  ------------------------------
   --  This `Encoder` translates the (binary) input stream into
   --  a Base62 ascii stream.
   type Encoder is new Util.Encoders.Transformer with private;

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

   --  Create a base62 encoder.
   function Create_Encoder return Transformer_Access;

   --  ------------------------------
   --  Base62 decoder
   --  ------------------------------
   --  The `Decoder` decodes a Base62 ascii stream into a binary stream.
   type Decoder is new Util.Encoders.Transformer with private;

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
   overriding
   procedure Transform (E       : in out Decoder;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset);

   --  Finish decoding the input array.  The `Encoding_Error` exception is raised
   --  if the last block is not a valid base62 block.
   overriding
   procedure Finish (E    : in out Decoder;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset);

   --  Create a base62 decoder.
   function Create_Decoder return Transformer_Access;

private

   --  Number of bytes of a binary block and number of characters to encode it.
   BLOCK_LENGTH  : constant := 8;
   ENCODE_LENGTH : constant := 11;

   type Alphabet is
     array (Interfaces.Unsigned_64 range 0 .. 61) of Ada.Streams.Stream_Element;

   BASE62_ALPHABET : constant Alphabet :=
     (Character'Pos ('0'), Character'Pos ('1'), Character'Pos ('2'), Character'Pos ('3'),
      Character'Pos ('4'), Character'Pos ('5'), Character'Pos ('6'), Character'Pos ('7'),
      Character'Pos ('8'), Character'Pos ('9'), Character'Pos ('A'), Character'Pos ('B'),
      Character'Pos ('C'), Character'Pos ('D'), Character'Pos ('E'), Character'Pos ('F'),
      Character'Pos ('G'), Character'Pos ('H'), Character'Pos ('I'), Character'Pos ('J'),
      Character'Pos ('K'), Character'Pos ('L'), Character'Pos ('M'), Character'Pos ('N'),
      Character'Pos ('O'), Character'Pos ('P'), Character'Pos ('Q'), Character'Pos ('R'),
      Character'Pos ('S'), Character'Pos ('T'), Character'Pos ('U'), Character'Pos ('V'),
      Character'Pos ('W'), Character'Pos ('X'), Character'Pos ('Y'), Character'Pos ('Z'),
      Character'Pos ('a'), Character'Pos ('b'), Character'Pos ('c'), Character'Pos ('d'),
      Character'Pos ('e'), Character'Pos ('f'), Character'Pos ('g'), Character'Pos ('h'),
      Character'Pos ('i'), Character'Pos ('j'), Character'Pos ('k'), Character'Pos ('l'),
      Character'Pos ('m'), Character'Pos ('n'), Character'Pos ('o'), Character'Pos ('p'),
      Character'Pos ('q'), Character'Pos ('r'), Character'Pos ('s'), Character'Pos ('t'),
      Character'Pos ('u'), Character'Pos ('v'), Character'Pos ('w'), Character'Pos ('x'),
      Character'Pos ('y'), Character'Pos ('z'));

   --  Number of bytes of the current block already collected by the encoder.
   type Encode_State_Type is new Natural range 0 .. BLOCK_LENGTH - 1;

   --  Number of characters used to encode a last block of 1 to 7 bytes.
   type Encode_Length_Array is
     array (Encode_State_Type range 1 .. Encode_State_Type'Last)
     of Ada.Streams.Stream_Element_Offset;

   ENCODE_LENGTHS : constant Encode_Length_Array := (2, 3, 5, 6, 7, 9, 10);

   type Encoder is new Util.Encoders.Transformer with record
      Value : Interfaces.Unsigned_64 := 0;
      State : Encode_State_Type := 0;
   end record;

   type Alphabet_Values is array (Ada.Streams.Stream_Element) of Interfaces.Unsigned_8;

   BASE62_VALUES : constant Alphabet_Values :=
     (Character'Pos ('0') => 0,  Character'Pos ('1') => 1,
      Character'Pos ('2') => 2,  Character'Pos ('3') => 3,
      Character'Pos ('4') => 4,  Character'Pos ('5') => 5,
      Character'Pos ('6') => 6,  Character'Pos ('7') => 7,
      Character'Pos ('8') => 8,  Character'Pos ('9') => 9,
      Character'Pos ('A') => 10, Character'Pos ('B') => 11,
      Character'Pos ('C') => 12, Character'Pos ('D') => 13,
      Character'Pos ('E') => 14, Character'Pos ('F') => 15,
      Character'Pos ('G') => 16, Character'Pos ('H') => 17,
      Character'Pos ('I') => 18, Character'Pos ('J') => 19,
      Character'Pos ('K') => 20, Character'Pos ('L') => 21,
      Character'Pos ('M') => 22, Character'Pos ('N') => 23,
      Character'Pos ('O') => 24, Character'Pos ('P') => 25,
      Character'Pos ('Q') => 26, Character'Pos ('R') => 27,
      Character'Pos ('S') => 28, Character'Pos ('T') => 29,
      Character'Pos ('U') => 30, Character'Pos ('V') => 31,
      Character'Pos ('W') => 32, Character'Pos ('X') => 33,
      Character'Pos ('Y') => 34, Character'Pos ('Z') => 35,
      Character'Pos ('a') => 36, Character'Pos ('b') => 37,
      Character'Pos ('c') => 38, Character'Pos ('d') => 39,
      Character'Pos ('e') => 40, Character'Pos ('f') => 41,
      Character'Pos ('g') => 42, Character'Pos ('h') => 43,
      Character'Pos ('i') => 44, Character'Pos ('j') => 45,
      Character'Pos ('k') => 46, Character'Pos ('l') => 47,
      Character'Pos ('m') => 48, Character'Pos ('n') => 49,
      Character'Pos ('o') => 50, Character'Pos ('p') => 51,
      Character'Pos ('q') => 52, Character'Pos ('r') => 53,
      Character'Pos ('s') => 54, Character'Pos ('t') => 55,
      Character'Pos ('u') => 56, Character'Pos ('v') => 57,
      Character'Pos ('w') => 58, Character'Pos ('x') => 59,
      Character'Pos ('y') => 60, Character'Pos ('z') => 61,
      others => 16#FF#);

   --  Number of characters of the current block already collected by the decoder.
   type Decode_State_Type is new Natural range 0 .. ENCODE_LENGTH - 1;

   --  Number of bytes encoded by a last block of 1 to 10 characters
   --  (0 when a block of that length is not valid).
   type Decode_Length_Array is
     array (Decode_State_Type range 1 .. Decode_State_Type'Last)
     of Ada.Streams.Stream_Element_Offset;

   DECODE_LENGTHS : constant Decode_Length_Array := (0, 1, 2, 0, 3, 4, 5, 0, 6, 7);

   type Decoder is new Util.Encoders.Transformer with record
      Value : Interfaces.Unsigned_64 := 0;
      State : Decode_State_Type := 0;
   end record;

end Util.Encoders.Base62;
