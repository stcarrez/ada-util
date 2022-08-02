-----------------------------------------------------------------------
--  util-http-mimes -- HTTP Headers
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
with Util.Strings;
package Util.Http.Mimes is

   subtype Mime_Access is Util.Strings.Name_Access;

   type Mime_List is array (Positive range <>) of Mime_Access;
   type Mime_List_Access is access constant Mime_List;

   Json  : aliased constant String := "application/json";
   Xml   : aliased constant String := "application/xml";
   Pdf   : aliased constant String := "application/pdf";
   Form  : aliased constant String := "application/x-www-form-urlencoded";
   Text  : aliased constant String := "text/plain";
   Html  : aliased constant String := "text/html";
   Css   : aliased constant String := "text/css";
   Js    : aliased constant String := "text/javascript";
   Png   : aliased constant String := "image/png";
   Jpg   : aliased constant String := "image/jpeg";
   Gif   : aliased constant String := "image/gif";
   Ico   : aliased constant String := "image/x-icon";
   Svg   : aliased constant String := "image/svg+xml";
   Octet : aliased constant String := "application/octet-stream";

   --  List of mime types for images.
   Images : aliased constant Mime_List
     := (Jpg'Access, Png'Access, Gif'Access, Ico'Access, Svg'Access);

   --  List of mime types for HTTP responses.
   Api    : aliased constant Mime_List := (Json'Access, Xml'Access);

   --  Returns true if the Content-Type header uses the given mime type.
   --  The `Header` parameter is assumed to follow the media type specification
   --  with the pattern:
   --    type / subtype [; token = (token|quoted-string)]
   function Is_Mime (Header : in String;
                     Mime   : in String) return Boolean;

end Util.Http.Mimes;
