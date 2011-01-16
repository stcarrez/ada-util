-----------------------------------------------------------------------
--  mapping -- Example of record mappings
--  Copyright (C) 2010, 2011 Stephane Carrez
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

with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;
package body Mapping is

   use Util.Beans.Objects;

   type Person_Fields is (FIELD_FIRST_NAME, FIELD_LAST_NAME, FIELD_AGE);

   type Person_Access is access all Person;

   --  Set the name/value pair on the current object.
   procedure Set_Member (P     : in out Person;
                         Field : in Person_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_FIRST_NAME =>
            P.First_Name := To_Unbounded_String (Value);

         when FIELD_LAST_NAME =>
            P.Last_Name := To_Unbounded_String (Value);

         when FIELD_AGE =>
            P.Age := To_Integer (Value);

      end case;
   end Set_Member;

   type Address_Fields is (FIELD_CITY, FIELD_STREET, FIELD_COUNTRY, FIELD_ZIP);
   type Address_Access is access all Address;

   --  Set the name/value pair on the current object.
   procedure Set_Member (Addr  : in out Address;
                         Field : in Address_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
       case Field is
         when FIELD_CITY =>
            Addr.City := To_Unbounded_String (Value);

         when FIELD_STREET =>
            Addr.Street := To_Unbounded_String (Value);

         when FIELD_COUNTRY =>
            Addr.Country := To_Unbounded_String (Value);

         when FIELD_ZIP =>
            Addr.Zip := To_Integer (Value);

      end case;
   end Set_Member;

   package Address_Mapper is new Util.Serialize.Mappers.Record_Mapper
     (Element_Type => Address, Element_Type_Access => Address_Access,
      Fields => Address_Fields, Set_Member => Set_Member);

   package Person_Mapper is new Util.Serialize.Mappers.Record_Mapper
     (Element_Type => Person, Element_Type_Access => Person_Access,
      Fields => Person_Fields, Set_Member => Set_Member);

   Address_Mapping : aliased Address_Mapper.Mapper;

   Person_Mapping  : aliased Person_Mapper.Mapper;


   --  ------------------------------
   --  Get the address mapper which describes how to load an Address.
   --  ------------------------------
   function Get_Address_Mapper return Util.Serialize.Mappers.Mapper_Access is
   begin
      return Address_Mapping'Access;
   end Get_Address_Mapper;

   --  ------------------------------
   --  Get the person mapper which describes how to load a Person.
   --  ------------------------------
   function Get_Person_Mapper return Util.Serialize.Mappers.Mapper_Access is
   begin
      return Person_Mapping'Access;
   end Get_Person_Mapper;

begin
   --  XML:                                JSON:
   --  ----                                -----
   --  <city>Paris</city>                  "city" : "Paris"
   --  <street>Champs de Mars</street>     "street" : "Champs de Mars"
   --  <country>France</country>           "country" : "France"
   --  <zip>75</zip>                       "zip" : 75
   Address_Mapping.Add_Mapping ("city", FIELD_CITY);
   Address_Mapping.Add_Mapping ("street", FIELD_STREET);
   Address_Mapping.Add_Mapping ("country", FIELD_COUNTRY);
   Address_Mapping.Add_Mapping ("zip", FIELD_ZIP);

   --  XML:
   --  ----
   --  <xxx id='23'>
   --     <address>
   --        <city>...</city>
   --        <street number='44'>..</street>
   --        <country>..</country>
   --        <zip>..</zip>
   --     </address>
   --     <name>John</name>
   --     <last_name>Harry</last_name>
   --     <age>23</age>
   --     <info><facebook><id>...</id></facebook></info>
   --  </xxx>
   --  Person_Mapper.Add_Mapping ("@id");
   --  Person_Mapper.Add_Mapping ("address", Address_Mapper'Access);
   Person_Mapping.Add_Mapping ("name", FIELD_FIRST_NAME);
   Person_Mapping.Add_Mapping ("last_name", FIELD_LAST_NAME);
   Person_Mapping.Add_Mapping ("age", FIELD_AGE);
  --   Person_Mapper.Add_Mapping ("info/*");
   --  Person_Mapper.Add_Mapping ("address/street/@number");

end Mapping;
