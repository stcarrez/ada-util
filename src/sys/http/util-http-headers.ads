-----------------------------------------------------------------------
--  util-http-headers -- HTTP Headers
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
with Util.Dates.RFC7231;

with Util.Http.Mimes;
package Util.Http.Headers is

   --  Some header names.
   Content_Type    : constant String := "Content-Type";
   Content_Length  : constant String := "Content-Length";
   Accept_Header   : constant String := "Accept";
   Accept_Language : constant String := "Accept-Language";
   Location        : constant String := "Location";
   Cookie          : constant String := "Cookie";
   Cache_Control   : constant String := "Cache-Control";

   function To_Header (Date : in Ada.Calendar.Time) return String
     renames Util.Dates.RFC7231.Image;

   type Quality_Type is digits 4 range 0.0 .. 1.0;

   --  Split an accept like header into multiple tokens and a quality value.
   --  Invoke the `Process` procedure for each token.  Example:
   --
   --     Accept-Language: de, en;q=0.7, jp, fr;q=0.8, ru
   --
   --  The `Process` will be called for "de", "en" with quality 0.7,
   --  and "jp", "fr" with quality 0.8 and then "ru" with quality 1.0.
   procedure Split_Header (Header  : in String;
                           Process : access procedure (Item    : in String;
                                                       Quality : in Quality_Type));

   --  Get the accepted media according to the `Accept` header value and a list
   --  of media/mime types.  The quality matching and wildcard are handled
   --  so that we return the best match.  With the following HTTP header:
   --
   --     Accept: image/*; q=0.2, image/jpeg
   --
   --  and if the mimes list contains `image/png` but not `image/jpeg`, the
   --  first one is returned.  If the list contains both, then `image/jpeg` is
   --  returned.
   function Get_Accepted (Header : in String;
                          Mimes  : in Util.Http.Mimes.Mime_List)
                         return Util.Http.Mimes.Mime_Access;

end Util.Http.Headers;
