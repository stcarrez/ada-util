-----------------------------------------------------------------------
--  mapping -- Example of serialization mappings
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
with Util.Serialize.Contexts;
with Util.Beans.Objects;
package body Mapping is

   use Util.Beans.Objects;

   --  ------------------------------
   --  Set the name/value pair on the current object.
   --  ------------------------------
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

   --  ------------------------------
   --  Set the name/value pair on the current object.
   --  ------------------------------
   function Get_Person_Member (From  : in Person;
                               Field : in Person_Fields) return Util.Beans.Objects.Object is
   begin
      case Field is
         when FIELD_FIRST_NAME =>
            return Util.Beans.Objects.To_Object (From.First_Name);

         when FIELD_LAST_NAME =>
            return Util.Beans.Objects.To_Object (From.Last_Name);

         when FIELD_AGE =>
            return Util.Beans.Objects.To_Object (From.Age);

      end case;
   end Get_Person_Member;

   type Address_Fields is (FIELD_CITY, FIELD_STREET, FIELD_COUNTRY, FIELD_ZIP);
   type Address_Access is access all Address;

   --  ------------------------------
   --  Set the name/value pair on the current object.
   --  ------------------------------
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

   function Get_Member (Addr  : in Address;
                        Field : in Address_Fields) return Util.Beans.Objects.Object is
   begin
      case Field is
         when FIELD_CITY =>
            return Util.Beans.Objects.To_Object (Addr.City);

         when FIELD_STREET =>
            return Util.Beans.Objects.To_Object (Addr.Street);

         when FIELD_COUNTRY =>
            return Util.Beans.Objects.To_Object (Addr.Country);

         when FIELD_ZIP =>
            return Util.Beans.Objects.To_Object (Addr.Zip);

      end case;
   end Get_Member;

   package Address_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Address,
                                               Element_Type_Access => Address_Access,
                                               Fields              => Address_Fields,
                                               Set_Member          => Set_Member);

   --  Mapping for the Address record.
   Address_Mapping       : aliased Address_Mapper.Mapper;

   --  Mapping for the Person record.
   Person_Mapping        : aliased Person_Mapper.Mapper;

   --  Mapping for a list of Person records (stored as a Vector).
   Person_Vector_Mapping : aliased Person_Vector_Mapper.Mapper;

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
   function Get_Person_Mapper return Person_Mapper.Mapper_Access is
   begin
      return Person_Mapping'Access;
   end Get_Person_Mapper;

   --  ------------------------------
   --  Get the person vector mapper which describes how to load a list of Person.
   --  ------------------------------
   function Get_Person_Vector_Mapper return Person_Vector_Mapper.Mapper_Access is
   begin
      return Person_Vector_Mapping'Access;
   end Get_Person_Vector_Mapper;

   --  ------------------------------
   --  Helper to give access to the <b>Address</b> member of a <b>Person</b>.
   --  ------------------------------
   procedure Person_Address (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                             Process : not null access procedure (Item : in out Address)) is
      procedure Process_Person (P : in out Person) is
      begin
         Process (P.Addr);
      end Process_Person;
   begin
      Person_Mapper.Execute_Object (Ctx, Process_Person'Access);
   end Person_Address;

   --  ------------------------------
   --  Helper to give access to the <b>Address</b> member of a <b>Person</b>.
   --  ------------------------------
--     procedure Proxy_Person_Address (Element : in out Person;
--                                     Process : not null access procedure (Item : in out Address)) is
--     begin
--        Process (Element.Addr);
--     end Proxy_Person_Address;

begin
   --  XML:                                JSON:
   --  ----                                -----
   --  <city>Paris</city>                  "city" : "Paris"
   --  <street>Champs de Mars</street>     "street" : "Champs de Mars"
   --  <country>France</country>           "country" : "France"
   --  <zip>75</zip>                       "zip" : 75
   Address_Mapping.Bind (Person_Address'Access);
   Address_Mapping.Bind (Get_Member'Access);
   Address_Mapping.Add_Default_Mapping;

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
   Person_Mapping.Bind (Get_Person_Member'Access);
   Person_Mapping.Add_Mapping ("address", Address_Mapping'Access);
   Person_Mapping.Add_Mapping ("name", FIELD_FIRST_NAME);
   Person_Mapping.Add_Default_Mapping;
  --   Person_Mapper.Add_Mapping ("info/*");
   --  Person_Mapper.Add_Mapping ("address/street/@number");

   Person_Vector_Mapping.Set_Mapping ("person", Person_Mapping);
end Mapping;
