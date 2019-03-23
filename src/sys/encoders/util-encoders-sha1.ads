-----------------------------------------------------------------------
--  util-encoders-sha1 -- Compute SHA-1 hash
--  Copyright (C) 2011, 2017, 2019 Stephane Carrez
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
with Ada.Finalization;
with Interfaces;

--  The <b>Util.Encodes.SHA1</b> package generates SHA-1 hash according to
--  RFC3174 or [FIPS-180-1].
package Util.Encoders.SHA1 is

   pragma Preelaborate;

   HASH_SIZE : constant := 20;

   --  The SHA-1 binary hash (160-bit).
   subtype Hash_Array is Ada.Streams.Stream_Element_Array (0 .. HASH_SIZE - 1);

   --  The SHA-1 hash as hexadecimal string.
   subtype Digest is String (1 .. 2 * HASH_SIZE);

   subtype Base64_Digest is String (1 .. 28);

   --  ------------------------------
   --  SHA1 Context
   --  ------------------------------
   type Context is limited private;

   --  Update the hash with the string.
   procedure Update (E : in out Context;
                     S : in String);

   --  Update the hash with the string.
   procedure Update (E : in out Context;
                     S : in Ada.Streams.Stream_Element_Array);

   --  Computes the SHA1 hash and returns the raw binary hash in <b>Hash</b>.
   procedure Finish (E    : in out Context;
                     Hash : out Hash_Array);

   --  Computes the SHA1 hash and returns the hexadecimal hash in <b>Hash</b>.
   procedure Finish (E    : in out Context;
                     Hash : out Digest);

   --  Computes the SHA1 hash and returns the base64 hash in <b>Hash</b>.
   procedure Finish_Base64 (E    : in out Context;
                            Hash : out Base64_Digest);

   --  ------------------------------
   --  SHA1 encoder
   --  ------------------------------
   --  This <b>Encoder</b> translates the (binary) input stream into
   --  an SHA1 hexadecimal stream.  The encoding alphabet is: 0123456789ABCDEF.
   type Encoder is new Util.Encoders.Transformer with private;

   --  Encodes the binary input stream represented by <b>Data</b> into
   --  an SHA-1 hash output stream <b>Into</b>.
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

private

   type Encoder is new Util.Encoders.Transformer with null record;

   use Interfaces;

   type H_Array is array (0 .. 4) of Unsigned_32;
   type W_Array is array (0 .. 79) of Unsigned_32;

   --  Process the message block collected in the context.
   procedure Compute (Ctx : in out Context);

   type Context is new Ada.Finalization.Limited_Controlled with record
      W           : W_Array;
      H           : H_Array;
      Pos         : Natural;
      Count       : Unsigned_64;
      Pending     : String (1 .. 3);
      Pending_Pos : Natural;
   end record;

   --  Initialize the SHA-1 context.
   overriding
   procedure Initialize (E : in out Context);

end Util.Encoders.SHA1;
