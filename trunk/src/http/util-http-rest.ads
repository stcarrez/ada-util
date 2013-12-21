-----------------------------------------------------------------------
--  util-http-rest -- REST API support
--  Copyright (C) 2012, 2013 Stephane Carrez
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

with Util.Serialize.IO;
with Util.Http.Clients;
with Util.Serialize.Mappers.Record_Mapper;

--  The <b>Util.Http.Rest</b> package defines a REST client type which helps in writing
--  REST client APIs.  A REST client is similar to an HTTP client but it provides additional
--  operations that are useful in REST APIs.
package Util.Http.Rest is

   --  -----------------------
   --  REST client
   --  -----------------------
   type Client is new Util.Http.Clients.Client with private;

   --  Execute an HTTP GET operation using the <b>Http</b> client on the given <b>URI</b>.
   --  Upon successful reception of the response, parse the JSON result and populate the
   --  serialization context associated with the parser.
   procedure Get (Http   : in out Client;
                  URI    : in String;
                  Parser : in out Util.Serialize.IO.Parser'Class);

   --  Execute an HTTP GET operation on the given <b>URI</b> and parse the JSON response
   --  into the target object refered to by <b>Into</b> by using the mapping described
   --  in <b>Mapping</b>.
   generic
      --  Package that maps the element into a record.
      with package Element_Mapper is
        new Util.Serialize.Mappers.Record_Mapper (<>);
   procedure Rest_Get (URI     : in String;
                       Mapping : in Util.Serialize.Mappers.Mapper_Access;
                       Path    : in String := "";
                       Into    : in Element_Mapper.Element_Type_Access);

private

   type Client is new Util.Http.Clients.Client with record
      Status : Natural := 0;
   end record;

end Util.Http.Rest;
