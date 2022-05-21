-----------------------------------------------------------------------
--  util-encoders-uri -- Encode and decode URI using percent encoding
--  Copyright (C) 2022 Stephane Carrez
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

--  == URI Encoder and Decoder ==
--  The `Util.Encoders.URI` package provides operations to encode and decode
--  using the URI percent encoding and decoding scheme.
--  A string encoded using percent encoding as described in RFC 3986 is
--  simply decoded as follows:
--
--    Decoded : constant String := Util.Encoders.URI.Decode ("%20%2F%3A");
--
--  To encode a string, one must choose the character set that must be encoded
--  and then call the `Encode` function.  The character set indicates those
--  characters that must be percent encoded.  Two character sets are provided,
--
--  * `HREF_STRICT` defines a strict character set to encode all reserved
--    characters as defined by RFC 3986.  This is the default.
--  * `HREF_LOOSE` defines a character set that does not encode the
--    reserved characters such as `-_.+!*'(),%#@?=;:/&$`.
--
--    Encoded : constant String := Util.Encoders.URI.Encode (" /:");
--
package Util.Encoders.URI is

   pragma Preelaborate;

   type Encoding_Array is array (Character) of Boolean;

   --  Strict encoding of reserved characters RFC3986
   HREF_STRICT : constant Encoding_Array
     := ('0' .. '9' => False,
         'a' .. 'z' => False,
         'A' .. 'Z' => False,
         '-' => False, '.' => False, '_' => False, '~' => False,
         others => True);

   --  Loose encoding where punctuation reserved characters are not encoded.
   HREF_LOOSE  : constant Encoding_Array
     := ('0' .. '9' => False,
         'a' .. 'z' => False,
         'A' .. 'Z' => False,
         '-' => False, '.' => False, '_' => False, '~' => False, '+' => False,
         ''' => False, '*' => False, '(' => False, '&' => False, '$' => False,
         ')' => False, ',' => False, '%' => False, '#' => False, '@' => False,
         '?' => False, '=' => False, ';' => False, ':' => False, '/' => False,
         others => True);

   --  Compute the length of the encoded URI string with percent encoding
   --  and with the given encoding array.  Characters for which the `Encoding` array
   --  returns True are encoded using %HEXDIGIT HEXDIGIT.
   --  Returns the length of encoded string.
   function Encoded_Length (URI      : in String;
                            Encoding : in Encoding_Array := HREF_STRICT) return Natural;

   --  Encode the string using URI percent encoding.
   --  Characters for which the `Encoding` array returns True are encoded
   --  using %HEXDIGIT HEXDIGIT.  Returns the percent encoded string.
   function Encode (URI      : in String;
                    Encoding : in Encoding_Array := HREF_STRICT) return String;

   --  Decode the percent encoded URI string.
   function Decode (URI : in String) return String;

end Util.Encoders.URI;
