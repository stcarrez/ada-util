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

package body City_Mapping is

   use Util.Beans.Objects;

   --  ------------------------------
   --  Set the name/value pair on the current object.
   --  ------------------------------
   procedure Set_Member (P     : in out City;
                         Field : in City_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_COUNTRY =>
            P.Country := To_Unbounded_String (Value);

         when FIELD_CITY =>
            P.City := To_Unbounded_String (Value);

         when FIELD_NAME =>
            P.Name := To_Unbounded_String (Value);

         when FIELD_REGION =>
            P.Region := To_Unbounded_String (Value);

         when FIELD_LATITUDE =>
            P.Latitude := To_Float (Value);

         when FIELD_LONGITUDE =>
            P.Longitude := To_Float (Value);

      end case;
   end Set_Member;

   --  Mapping for the Person record.
   City_Mapping        : aliased City_Mapper.Mapper;

   --  Mapping for a list of City records (stored as a Vector).
   City_Vector_Mapping : aliased City_Vector_Mapper.Mapper;

   --  ------------------------------
   --  Get the address mapper which describes how to load an Address.
   --  ------------------------------
   function Get_City_Mapper return Util.Serialize.Mappers.Mapper_Access is
   begin
      return City_Mapping'Access;
   end Get_City_Mapper;

   --  ------------------------------
   --  Get the person vector mapper which describes how to load a list of Person.
   --  ------------------------------
   function Get_City_Vector_Mapper return City_Vector_Mapper.Mapper_Access is
   begin
      return City_Vector_Mapping'Access;
   end Get_City_Vector_Mapper;

begin
   City_Mapping.Add_Mapping ("Country", FIELD_COUNTRY);
   City_Mapping.Add_Mapping ("City", FIELD_CITY);
   City_Mapping.Add_Mapping ("Accent City", FIELD_NAME);
   City_Mapping.Add_Mapping ("Region", FIELD_REGION);
   City_Mapping.Add_Mapping ("Latitude", FIELD_LATITUDE);
   City_Mapping.Add_Mapping ("Longitude", FIELD_LONGITUDE);

   City_Vector_Mapping.Set_Mapping (City_Mapping'Access);
end City_Mapping;
