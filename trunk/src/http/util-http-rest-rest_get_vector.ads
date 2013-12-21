-----------------------------------------------------------------------
--  util-http-rest-rest_get_vector -- REST API support
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

with Util.Serialize.Mappers.Vector_Mapper;

--  Execute an HTTP GET operation on the given <b>URI</b> and parse the JSON response
--  into the target object refered to by <b>Into</b> by using the mapping described
--  in <b>Mapping</b>.
generic
   --  Package that maps the element into a vector of records.
   with package Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (<>);
procedure Util.Http.Rest.Rest_Get_Vector (URI     : in String;
                                          Mapping : in Util.Serialize.Mappers.Mapper_Access;
                                          Path    : in String := "";
                                          Into    : in Vector_Mapper.Vector_Type_Access);
