-----------------------------------------------------------------------
--  util-http-rest -- REST API support
--  Copyright (C) 2012, 2013, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Serialize.IO.JSON;
with Util.Serialize.Mappers;
package body Util.Http.Rest is

   --  -----------------------
   --  Execute an HTTP GET operation using the <b>Http</b> client on the given <b>URI</b>.
   --  Upon successful reception of the response, parse the JSON result and populate the
   --  serialization context associated with the parser.
   --  -----------------------
   procedure Get (Http   : in out Client;
                  URI    : in String;
                  Parser : in out Util.Serialize.IO.Parser'Class;
                  Sink   : in out Util.Serialize.IO.Reader'Class) is
      Response : Util.Http.Clients.Response;
   begin
      Http.Get (URI, Response);
      Http.Status := Response.Get_Status;
      declare
         Content      : constant String := Response.Get_Body;
      begin
         Parser.Parse_String (Content, Sink);
      end;
   end Get;

   --  -----------------------
   --  Execute an HTTP GET operation on the given <b>URI</b> and parse the JSON response
   --  into the target object referred to by <b>Into</b> by using the mapping described
   --  in <b>Mapping</b>.
   --  -----------------------
   procedure Rest_Get (URI     : in String;
                       Mapping : in Util.Serialize.Mappers.Mapper_Access;
                       Path    : in String := "";
                       Into    : in Element_Mapper.Element_Type_Access) is
      Http     : Util.Http.Rest.Client;
      Reader   : Util.Serialize.IO.JSON.Parser;
      Mapper   : Util.Serialize.Mappers.Processing;
   begin
      Mapper.Add_Mapping (Path, Mapping.all'Access);
      Element_Mapper.Set_Context (Mapper, Into);
      Http.Get (URI, Reader, Mapper);
   end Rest_Get;

end Util.Http.Rest;
