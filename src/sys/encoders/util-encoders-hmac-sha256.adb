-----------------------------------------------------------------------
--  util-encoders-hmac-sha1 -- Compute HMAC-SHA256 authentication code
--  Copyright (C) 2017, 2019 Stephane Carrez
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

with Util.Encoders.Base16;
with Util.Encoders.Base64;

package body Util.Encoders.HMAC.SHA256 is

   --  ------------------------------
   --  Sign the data string with the key and return the HMAC-SHA256 code in binary.
   --  ------------------------------
   function Sign (Key  : in String;
                  Data : in String) return Util.Encoders.SHA256.Hash_Array is
      Ctx    : Context;
      Result : Util.Encoders.SHA256.Hash_Array;
   begin
      Set_Key (Ctx, Key);
      Update (Ctx, Data);
      Finish (Ctx, Result);
      return Result;
   end Sign;

   --  ------------------------------
   --  Sign the data array with the key and return the HMAC-SHA256 code in the result.
   --  ------------------------------
   procedure Sign (Key    : in Ada.Streams.Stream_Element_Array;
                   Data   : in Ada.Streams.Stream_Element_Array;
                   Result : out Util.Encoders.SHA256.Hash_Array) is
      Ctx    : Context;
   begin
      Set_Key (Ctx, Key);
      Update (Ctx, Data);
      Finish (Ctx, Result);
   end Sign;

   procedure Sign (Key    : in Secret_Key;
                   Data   : in Ada.Streams.Stream_Element_Array;
                   Result : out Util.Encoders.SHA256.Hash_Array) is
   begin
      Sign (Key.Secret, Data, Result);
   end Sign;

   --  ------------------------------
   --  Sign the data string with the key and return the HMAC-SHA256 code as hexadecimal string.
   --  ------------------------------
   function Sign (Key  : in String;
                  Data : in String) return Util.Encoders.SHA256.Digest is
      Ctx    : Context;
      Result : Util.Encoders.SHA256.Digest;
   begin
      Set_Key (Ctx, Key);
      Update (Ctx, Data);
      Finish (Ctx, Result);
      return Result;
   end Sign;

   --  ------------------------------
   --  Sign the data string with the key and return the HMAC-SHA256 code as base64 string.
   --  When <b>URL</b> is True, use the base64 URL alphabet to encode in base64.
   --  ------------------------------
   function Sign_Base64 (Key  : in String;
                         Data : in String;
                         URL  : in Boolean := False) return Util.Encoders.SHA256.Base64_Digest is
      Ctx    : Context;
      Result : Util.Encoders.SHA256.Base64_Digest;
   begin
      Set_Key (Ctx, Key);
      Update (Ctx, Data);
      Finish_Base64 (Ctx, Result, URL);
      return Result;
   end Sign_Base64;

   --  ------------------------------
   --  Set the hmac private key.  The key must be set before calling any <b>Update</b>
   --  procedure.
   --  ------------------------------
   procedure Set_Key (E   : in out Context;
                      Key : in String) is
      Buf : Ada.Streams.Stream_Element_Array (1 .. Key'Length);
      for Buf'Address use Key'Address;
      pragma Import (Ada, Buf);

   begin
      Set_Key (E, Buf);
   end Set_Key;

   IPAD : constant Ada.Streams.Stream_Element := 16#36#;
   OPAD : constant Ada.Streams.Stream_Element := 16#5c#;

   --  ------------------------------
   --  Set the hmac private key.  The key must be set before calling any <b>Update</b>
   --  procedure.
   --  ------------------------------
   procedure Set_Key (E   : in out Context;
                      Key : in Ada.Streams.Stream_Element_Array) is
   begin
      --  Reduce the key
      if Key'Length > 64 then
         Util.Encoders.SHA256.Update (E.SHA, Key);
         Util.Encoders.SHA256.Finish (E.SHA, E.Key (0 .. 31));
         E.Key_Len := 31;
      else
         E.Key_Len := Key'Length - 1;
         E.Key (0 .. E.Key_Len) := Key;
      end if;

      --  Hash the key in the SHA256 context.
      declare
         Block : Ada.Streams.Stream_Element_Array (0 .. 63);
      begin
         for I in 0 .. E.Key_Len loop
            Block (I) := IPAD xor E.Key (I);
         end loop;
         for I in E.Key_Len + 1 .. 63 loop
            Block (I) := IPAD;
         end loop;
         Util.Encoders.SHA256.Update (E.SHA, Block);
      end;
   end Set_Key;

   procedure Set_Key (E   : in out Context;
                      Key : in Secret_Key) is
   begin
      Set_Key (E, Key.Secret);
   end Set_Key;

   --  ------------------------------
   --  Update the hash with the string.
   --  ------------------------------
   procedure Update (E : in out Context;
                     S : in String) is
   begin
      Util.Encoders.SHA256.Update (E.SHA, S);
   end Update;

   --  ------------------------------
   --  Update the hash with the string.
   --  ------------------------------
   procedure Update (E : in out Context;
                     S : in Ada.Streams.Stream_Element_Array) is
   begin
      Util.Encoders.SHA256.Update (E.SHA, S);
   end Update;

   --  ------------------------------
   --  Update the hash with the secret key.
   --  ------------------------------
   procedure Update (E : in out Context;
                     S : in Secret_Key) is
   begin
      Update (E, S.Secret);
   end Update;

   --  ------------------------------
   --  Computes the HMAC-SHA256 with the private key and the data collected by
   --  the <b>Update</b> procedures.  Returns the raw binary hash in <b>Hash</b>.
   --  ------------------------------
   procedure Finish (E    : in out Context;
                     Hash : out Util.Encoders.SHA256.Hash_Array) is
   begin
      Util.Encoders.SHA256.Finish (E.SHA, Hash);

      --  Hash the key in the SHA256 context.
      declare
         Block : Ada.Streams.Stream_Element_Array (0 .. 63);
      begin
         for I in 0 .. E.Key_Len loop
            Block (I) := OPAD xor E.Key (I);
         end loop;
         if E.Key_Len < 63 then
            for I in E.Key_Len + 1 .. 63 loop
               Block (I) := OPAD;
            end loop;
         end if;
         Util.Encoders.SHA256.Update (E.SHA, Block);
      end;
      Util.Encoders.SHA256.Update (E.SHA, Hash);
      Util.Encoders.SHA256.Finish (E.SHA, Hash);
   end Finish;

   --  ------------------------------
   --  Computes the HMAC-SHA256 with the private key and the data collected by
   --  the <b>Update</b> procedures.  Returns the hexadecimal hash in <b>Hash</b>.
   --  ------------------------------
   procedure Finish (E    : in out Context;
                     Hash : out Util.Encoders.SHA256.Digest) is
      H       : Util.Encoders.SHA256.Hash_Array;
      B       : Util.Encoders.Base16.Encoder;
   begin
      Finish (E, H);
      B.Convert (H, Hash);
   end Finish;

   --  ------------------------------
   --  Computes the HMAC-SHA256 with the private key and the data collected by
   --  the <b>Update</b> procedures.  Returns the base64 hash in <b>Hash</b>.
   --  When <b>URL</b> is True, use the base64 URL alphabet to encode in base64.
   --  ------------------------------
   procedure Finish_Base64 (E    : in out Context;
                            Hash : out Util.Encoders.SHA256.Base64_Digest;
                            URL  : in Boolean := False) is
      H       : Util.Encoders.SHA256.Hash_Array;
      B       : Util.Encoders.Base64.Encoder;
   begin
      Finish (E, H);
      B.Set_URL_Mode (URL);
      B.Convert (H, Hash);
   end Finish_Base64;

   --  Initialize the SHA-1 context.
   overriding
   procedure Initialize (E : in out Context) is
   begin
      null;
   end Initialize;

end Util.Encoders.HMAC.SHA256;
