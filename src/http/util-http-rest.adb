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

with Util.Serialize.IO.JSON;
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

   --  -----------------------
   --  Execute an HTTP GET operation on the given <b>URI</b> and parse the JSON response
   --  into the target object refered to by <b>Into</b> by using the mapping described
   --  in <b>Mapping</b>.
   --  -----------------------
   procedure Rest_Get (URI     : in String;
                       Mapping : in Util.Serialize.Mappers.Mapper_Access;
                       Into    : in Element_Mapper.Element_Type_Access) is
      Http     : Util.Http.Rest.Client;
      Reader   : Util.Serialize.IO.JSON.Parser;
   begin
      Reader.Add_Mapping ("", Mapping.all'Access);
      Element_Mapper.Set_Context (Reader, Into);
      Http.Get (URI, Reader);
   end Rest_Get;

end Util.Http.Rest;
