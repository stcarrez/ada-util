-----------------------------------------------------------------------
--  util-http-rest-rest_get_vector -- REST API support
--  Copyright (C) 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Serialize.Mappers.Vector_Mapper;

--  Execute an HTTP GET operation on the given <b>URI</b> and parse the JSON response
--  into the target object referred to by <b>Into</b> by using the mapping described
--  in <b>Mapping</b>.
generic
   --  Package that maps the element into a vector of records.
   with package Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (<>);
procedure Util.Http.Rest.Rest_Get_Vector (URI     : in String;
                                          Mapping : in Util.Serialize.Mappers.Mapper_Access;
                                          Path    : in String := "";
                                          Into    : in Vector_Mapper.Vector_Type_Access);
