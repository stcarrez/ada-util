-----------------------------------------------------------------------
--  util-encoders-sha256 -- Compute SHA-256 hash
--  Copyright (C) 2017 Stephane Carrez
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
