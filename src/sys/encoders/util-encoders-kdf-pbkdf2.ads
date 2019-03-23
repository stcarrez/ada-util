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

--  Password-Based Key Derivation Function 2, RFC 8018.
generic
   Length : Stream_Element_Offset;
   with procedure Hash (Key  : in Ada.Streams.Stream_Element_Array;
                        Data : in Ada.Streams.Stream_Element_Array;
                        Into : out Ada.Streams.Stream_Element_Array);
procedure Util.Encoders.KDF.PBKDF2 (Password : in Secret_Key;
                                    Salt     : in Secret_Key;
                                    Counter  : in Positive;
                                    Result   : out Secret_Key);
