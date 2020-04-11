-----------------------------------------------------------------------
--  util-encoders-quoted_printable -- Encode/Decode a stream in quoted-printable
--  Copyright (C) 2020 Stephane Carrez
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

package Util.Encoders.Quoted_Printable is

   pragma Preelaborate;

   --  Decode the Quoted-Printable string and return the result.
   --  When Strict is true, raises the Encoding_Error exception if the
   --  format is invalid.  Otherwise, ignore invalid encoding.
   function Decode (Content : in String;
                    Strict  : in Boolean := True) return String;

   --  Decode the "Q" encoding, similar to Quoted-Printable but with
   --  spaces that can be replaced by '_'.
   --  See RFC 2047.
   function Q_Decode (Content : in String) return String;

end Util.Encoders.Quoted_Printable;
