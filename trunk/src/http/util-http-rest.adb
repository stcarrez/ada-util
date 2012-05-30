-----------------------------------------------------------------------
--  util-http-rest -- REST API support
--  Copyright (C) 2012 Stephane Carrez
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

package body Util.Http.Rest is

   --  -----------------------
   --  Execute an HTTP GET operation using the <b>Http</b> client on the given <b>URI</b>.
   --  Upon successful reception of the response, parse the JSON result and populate the
   --  serialization context associated with the parser.
   --  -----------------------
   procedure Get (Http   : in out Client;
                  URI    : in String;
                  Parser : in out Util.Serialize.IO.Parser'Class) is
      Response : Util.Http.Clients.Response;
   begin
      Http.Get (URI, Response);
      Http.Status := Response.Get_Status;
      declare
         Content_Type : constant String := Response.Get_Header ("Content-Type");
         Content      : constant String := Response.Get_Body;
      begin
         Parser.Parse_String (Content);
      end;
   end Get;

end Util.Http.Rest;
