-----------------------------------------------------------------------
--  util-encoders-kdf-pbkdf2 -- Password-Based Key Derivation Function 2, RFC 8018.
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
with Util.Encoders.KDF.PBKDF2;
with Util.Encoders.HMAC.SHA256;
procedure Util.Encoders.KDF.PBKDF2_HMAC_SHA256 is
  new Util.Encoders.KDF.PBKDF2 (Length => Util.Encoders.HMAC.SHA256.HASH_SIZE,
                                Hash   => Util.Encoders.HMAC.SHA256.Sign);
