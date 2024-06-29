-----------------------------------------------------------------------
--  util-http-rest -- REST API support
--  Copyright (C) 2012, 2013, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Serialize.IO.JSON;

--  -----------------------
--  Execute an HTTP GET operation on the given <b>URI</b> and parse the JSON response
--  into the target object referred to by <b>Into</b> by using the mapping described
--  in <b>Mapping</b>.
--  -----------------------
procedure Util.Http.Rest.Rest_Get_Vector (URI     : in String;
                                     Mapping : in Util.Serialize.Mappers.Mapper_Access;
                                     Path    : in String := "";
                                     Into    : in Vector_Mapper.Vector_Type_Access) is
   Http     : Util.Http.Rest.Client;
   Reader   : Util.Serialize.IO.JSON.Parser;
   Mapper   : Util.Serialize.Mappers.Processing;
begin
   Mapper.Add_Mapping (Path, Mapping.all'Access);
   Vector_Mapper.Set_Context (Mapper, Into);
   Http.Get (URI, Reader, Mapper);
end Util.Http.Rest.Rest_Get_Vector;
