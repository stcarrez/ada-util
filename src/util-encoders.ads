-----------------------------------------------------------------------
--  util-encoders -- Encode/Decode streams and strings from one format to another
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Interfaces;

--  The <b>Util.Encoders</b> package defines the <b>Encoder</b> object
--  which represents a mechanism to transform a stream from one format into
--  another format.
package Util.Encoders is

   pragma Preelaborate;

   Not_Supported  : exception;
   Encoding_Error : exception;

   --  Encoder/decoder for Base64 (RFC 4648)
   BASE_64     : constant String := "base64";

   --  Encoder/decoder for Base64 (RFC 4648) using the URL alphabet
   --  (+ and / are replaced by - and _)
   BASE_64_URL : constant String := "base64url";

   --  Encoder/decoder for Base16 (RFC 4648)
   BASE_16     : constant String := "base16";
   HEX         : constant String := "hex";

   --  Encoder for SHA1 (RFC 3174)
   HASH_SHA1   : constant String := "sha1";

   --  ------------------------------
   --  Encoder context object
   --  ------------------------------
   --  The <b>Encoder</b> provides operations to encode and decode
   --  strings or stream of data from one format to another.
   --  The <b>Encoded</b> contains two <b>Transformer</b>
   --  which either <i>encodes</i> or <i>decodes</i> the stream.
   type Encoder is tagged limited private;

   --  Encodes the input string <b>Data</b> using the transformation
   --  rules provided by the <b>E</b> encoder.
   --
   --  Returns the encoded string.
   --
   --  Raises the <b>Encoding_Error</b> exception if the source string
   --  cannot be encoded.
   --  Raises the <b>Not_Supported</b> exception if the encoding is not
   --  supported.
   function Encode (E    : in Encoder;
                    Data : in String) return String;

   --  Decodes the input string <b>Data</b> using the transformation
   --  rules provided by the <b>E</b> encoder.
   --
   --  Returns the encoded string.
   --
   --  Raises the <b>Encoding_Error</b> exception if the source string
   --  cannot be decoded.
   --  Raises the <b>Not_Supported</b> exception if the decoding is not
   --  supported.
   function Decode (E    : in Encoder;
                    Data : in String) return String;

   --  Create the encoder object for the specified encoding format.
   function Create (Name : String) return Encoder;

   --  ------------------------------
   --  Stream Transformation interface
   --  ------------------------------
   --  The <b>Transformer</b> interface defines the operation to transform
   --  a stream from one data format to another.
   type Transformer is limited interface;
   type Transformer_Access is access all Transformer'Class;

   --  Transform the input stream represented by <b>Data</b> into
   --  the output stream <b>Into</b>.  The transformation made by
   --  the object can be of any nature (Hex encoding, Base64 encoding,
   --  Hex decoding, Base64 decoding, ...).
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
   procedure Transform (E       : in Transformer;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is abstract;

   procedure Transform (E    : in Transformer;
                        Data : in String;
                        Result : out Ada.Strings.Unbounded.Unbounded_String) is null;

   --  Transform the input string <b>Data</b> using the transformation
   --  rules provided by the <b>E</b> transformer.
   --
   --  Returns the transformed string.
   --
   --  Raises the <b>Encoding_Error</b> exception if the source string
   --  cannot be transformed
   function Transform (E    : in Transformer'Class;
                       Data : in String) return String;

   --  Transform the input string <b>Data</b> using the transformation
   --  rules provided by the <b>E</b> transformer.
   --
   --  Returns the transformed string.
   --
   --  Raises the <b>Encoding_Error</b> exception if the source string
   --  cannot be transformed
   function Transform (E    : in Transformer'Class;
                       Data : in Ada.Streams.Stream_Element_Array) return String;

   --  Encode the value represented by <tt>Val</tt> in the stream array <tt>Into</tt> starting
   --  at position <tt>Pos</tt> in that array.  The value is encoded using LEB128 format, 7-bits
   --  at a time until all non zero bits are written.  The <tt>Last</tt> parameter is updated
   --  to indicate the position of the last valid byte written in <tt>Into</tt>.
   procedure Encode_LEB128 (Into  : in out Ada.Streams.Stream_Element_Array;
                            Pos   : in Ada.Streams.Stream_Element_Offset;
                            Val   : in Interfaces.Unsigned_64;
                            Last  : out Ada.Streams.Stream_Element_Offset);

   --  Decode from the byte array <tt>From</tt> the value represented as LEB128 format starting
   --  at position <tt>Pos</tt> in that array.  After decoding, the <tt>Last</tt> index is updated
   --  to indicate the last position in the byte array.
   procedure Decode_LEB128 (From  : in Ada.Streams.Stream_Element_Array;
                            Pos   : in Ada.Streams.Stream_Element_Offset;
                            Val   : out Interfaces.Unsigned_64;
                            Last  : out Ada.Streams.Stream_Element_Offset);

private

   type Encoder is new Ada.Finalization.Limited_Controlled with record
      Encode : Transformer_Access := null;
      Decode : Transformer_Access := null;
   end record;

   --  Delete the transformers
   overriding
   procedure Finalize (E : in out Encoder);

end Util.Encoders;
