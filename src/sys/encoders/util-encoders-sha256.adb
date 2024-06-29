-----------------------------------------------------------------------
--  util-encoders-sha256 -- Compute SHA-256 hash
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Encoders.Base64;

package body Util.Encoders.SHA256 is

   --  ------------------------------
   --  Computes the SHA256 hash and returns the raw binary hash in <b>Hash</b>.
   --  ------------------------------
   procedure Finish (E    : in out Context;
                     Hash : out Hash_Array) is
   begin
      Hash := GNAT.SHA256.Digest (E);
      E := GNAT.SHA256.Initial_Context;
   end Finish;

   --  ------------------------------
   --  Computes the SHA256 hash and returns the hexadecimal hash in <b>Hash</b>.
   --  ------------------------------
   procedure Finish (E    : in out Context;
                     Hash : out Digest) is
   begin
      Hash := GNAT.SHA256.Digest (E);
      E := GNAT.SHA256.Initial_Context;
   end Finish;

   --  ------------------------------
   --  Computes the SHA256 hash and returns the base64 hash in <b>Hash</b>.
   --  ------------------------------
   procedure Finish_Base64 (E    : in out Context;
                            Hash : out Base64_Digest) is
      H       : Hash_Array;
      B       : Util.Encoders.Base64.Encoder;
   begin
      Finish (E, H);
      B.Convert (H, Hash);
      E := GNAT.SHA256.Initial_Context;
   end Finish_Base64;

end Util.Encoders.SHA256;
