-----------------------------------------------------------------------
--  util-encoders-sha256 -- Compute SHA-256 hash
--  Copyright (C) 2017, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams;
with GNAT.SHA256;

package Util.Encoders.SHA256 is

   HASH_SIZE : constant := 32;

   --  The SHA-256 binary hash (256-bit).
   subtype Hash_Array is Ada.Streams.Stream_Element_Array (0 .. HASH_SIZE - 1);

   --  The SHA-256 hash as hexadecimal string.
   subtype Digest is String (1 .. 2 * HASH_SIZE);

   subtype Base64_Digest is String (1 .. 44);

   --  ------------------------------
   --  SHA256 Context
   --  ------------------------------
   subtype Context is GNAT.SHA256.Context;

   --  Update the hash with the string.
   procedure Update (E : in out Context;
                     S : in String) renames GNAT.SHA256.Update;

   --  Update the hash with the string.
   procedure Update (E : in out Context;
                     S : in Ada.Streams.Stream_Element_Array) renames GNAT.SHA256.Update;

   --  Computes the SHA256 hash and returns the raw binary hash in <b>Hash</b>.
   procedure Finish (E    : in out Context;
                     Hash : out Hash_Array);

   --  Computes the SHA256 hash and returns the hexadecimal hash in <b>Hash</b>.
   procedure Finish (E    : in out Context;
                     Hash : out Digest);

   --  Computes the SHA256 hash and returns the base64 hash in <b>Hash</b>.
   procedure Finish_Base64 (E    : in out Context;
                            Hash : out Base64_Digest);

end Util.Encoders.SHA256;
