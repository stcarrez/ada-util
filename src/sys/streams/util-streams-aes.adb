-----------------------------------------------------------------------
--  util-streams-aes -- AES encoding and decoding streams
--  Copyright (C) 2019 Stephane Carrez
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

package body Util.Streams.AES is

   --  -----------------------
   --  Set the encryption key and mode to be used.
   --  -----------------------
   procedure Set_Key (Stream : in out Encoding_Stream;
                      Secret : in Util.Encoders.Secret_Key;
                      Mode   : in Util.Encoders.AES.AES_Mode := Util.Encoders.AES.CBC) is
   begin
      Stream.Transform.Set_Key (Secret, Mode);
   end Set_Key;

   --  -----------------------
   --  Set the encryption initialization vector before starting the encryption.
   --  -----------------------
   procedure Set_IV (Stream  : in out Encoding_Stream;
                     IV      : in Util.Encoders.AES.Word_Block_Type) is
   begin
      Stream.Transform.Set_IV (IV);
   end Set_IV;

   --  -----------------------
   --  Set the encryption key and mode to be used.
   --  -----------------------
   procedure Set_Key (Stream : in out Decoding_Stream;
                      Secret : in Util.Encoders.Secret_Key;
                      Mode   : in Util.Encoders.AES.AES_Mode := Util.Encoders.AES.CBC) is
   begin
      Stream.Transform.Set_Key (Secret, Mode);
   end Set_Key;

   --  -----------------------
   --  Set the encryption initialization vector before starting the encryption.
   --  -----------------------
   procedure Set_IV (Stream  : in out Decoding_Stream;
                     IV      : in Util.Encoders.AES.Word_Block_Type) is
   begin
      Stream.Transform.Set_IV (IV);
   end Set_IV;

end Util.Streams.AES;
