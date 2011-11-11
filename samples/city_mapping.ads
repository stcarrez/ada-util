-----------------------------------------------------------------------
--  city_mapping -- Example of serialization mapping for city CSV records
--  Copyright (C) 2011 Stephane Carrez
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
with Ada.Strings.Unbounded;

with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.Mappers.Vector_Mapper;
with Util.Serialize.Mappers;
with Ada.Containers.Vectors;
package City_Mapping is

   use Ada.Strings.Unbounded;

   type City is record
      Country   : Unbounded_String;
      City      : Unbounded_String;
      Name      : Unbounded_String;
      Region    : Unbounded_String;
      Latitude  : Float;
      Longitude : Float;
   end record;
   type City_Access is access all City;

   type City_Fields is (FIELD_COUNTRY, FIELD_CITY, FIELD_NAME,
                        FIELD_REGION, FIELD_LATITUDE, FIELD_LONGITUDE);

   --  Set the name/value pair on the current object.
   procedure Set_Member (P     : in out City;
                         Field : in City_Fields;
                         Value : in Util.Beans.Objects.Object);

   package City_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => City,
                                               Element_Type_Access => City_Access,
                                               Fields              => City_Fields,
                                               Set_Member          => Set_Member);

   subtype City_Context is City_Mapper.Element_Data;

   package City_Vector is
     new Ada.Containers.Vectors (Element_Type => City,
                                 Index_Type   => Positive);

   package City_Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (Vectors        => City_Vector,
                                               Element_Mapper => City_Mapper);

   subtype City_Vector_Context is City_Vector_Mapper.Vector_Data;

   --  Get the address mapper which describes how to load an Address.
   function Get_City_Mapper return Util.Serialize.Mappers.Mapper_Access;

   --  Get the person vector mapper which describes how to load a list of Person.
   function Get_City_Vector_Mapper return City_Vector_Mapper.Mapper_Access;

end City_Mapping;
