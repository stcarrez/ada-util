-----------------------------------------------------------------------
--  mapping -- Example of serialization mappings
--  Copyright (C) 2010, 2011, 2012, 2014 Stephane Carrez
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
with Util.Log.Loggers;
package body Mapping is

   use Util.Beans.Objects;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Mapping");

   type Property_Fields is (FIELD_NAME, FIELD_VALUE);
   type Property_Access is access all Property;

   type Address_Fields is (FIELD_CITY, FIELD_STREET, FIELD_COUNTRY, FIELD_ZIP);
   type Address_Access is access all Address;

   procedure Proxy_Address (Attr    : in Util.Serialize.Mappers.Mapping'Class;
                            Element : in out Person;
                            Value   : in Util.Beans.Objects.Object);
   function Get_Address_Member (Addr  : in Address;
                                Field : in Address_Fields) return Util.Beans.Objects.Object;
   procedure Set_Member (Addr  : in out Address;
                         Field : in Address_Fields;
                         Value : in Util.Beans.Objects.Object);
   procedure Set_Member (P : in out Property;
                         Field : in Property_Fields;
                         Value : in Util.Beans.Objects.Object);

   procedure Set_Member (P : in out Property;
                         Field : in Property_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_NAME =>
            P.Name := To_Unbounded_String (Value);

         when FIELD_VALUE =>
            P.Value := To_Unbounded_String (Value);

      end case;
   end Set_Member;

   package Property_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Property,
                                               Element_Type_Access => Property_Access,
                                               Fields              => Property_Fields,
                                               Set_Member          => Set_Member);

   --  ------------------------------
   --  Set the name/value pair on the current object.
   --  ------------------------------
   procedure Set_Member (P     : in out Person;
                         Field : in Person_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      Log.Debug ("Person field {0} - {0}", Person_Fields'Image (Field), To_String (Value));
      case Field is
         when FIELD_FIRST_NAME =>
            P.First_Name := To_Unbounded_String (Value);

         when FIELD_LAST_NAME =>
            P.Last_Name := To_Unbounded_String (Value);

         when FIELD_AGE =>
            P.Age := To_Integer (Value);

         when FIELD_ID =>
            P.Id := To_Long_Long_Integer (Value);

         when FIELD_NAME =>
            P.Name := To_Unbounded_String (Value);

         when FIELD_USER_NAME =>
            P.Username := To_Unbounded_String (Value);

         when FIELD_GENDER =>
            P.Gender := To_Unbounded_String (Value);

         when FIELD_LINK =>
            P.Link := To_Unbounded_String (Value);

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

         when FIELD_NAME =>
            return Util.Beans.Objects.To_Object (From.Name);

         when FIELD_USER_NAME =>
            return Util.Beans.Objects.To_Object (From.Username);

         when FIELD_LINK =>
            return Util.Beans.Objects.To_Object (From.Link);

         when FIELD_GENDER =>
            return Util.Beans.Objects.To_Object (From.Gender);

         when FIELD_ID =>
            return Util.Beans.Objects.To_Object (From.Id);

      end case;
   end Get_Person_Member;

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

   function Get_Address_Member (Addr  : in Address;
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
   end Get_Address_Member;

   package Address_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Address,
                                               Element_Type_Access => Address_Access,
                                               Fields              => Address_Fields,
                                               Set_Member          => Set_Member);

   --  Mapping for the Property record.
   Property_Mapping      : aliased Property_Mapper.Mapper;

   --  Mapping for the Address record.
   Address_Mapping       : aliased Address_Mapper.Mapper;

   --  Mapping for the Person record.
   Person_Mapping        : aliased Person_Mapper.Mapper;

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
   --  Helper to give access to the <b>Address</b> member of a <b>Person</b>.
   --  ------------------------------
   procedure Proxy_Address (Attr    : in Util.Serialize.Mappers.Mapping'Class;
                            Element : in out Person;
                            Value   : in Util.Beans.Objects.Object) is
   begin
      Address_Mapper.Set_Member (Attr, Element.Addr, Value);
   end Proxy_Address;

begin
   Property_Mapping.Add_Default_Mapping;

   --  XML:                                JSON:
   --  ----                                -----
   --  <city>Paris</city>                  "city" : "Paris"
   --  <street>Champs de Mars</street>     "street" : "Champs de Mars"
   --  <country>France</country>           "country" : "France"
   --  <zip>75</zip>                       "zip" : 75
   Address_Mapping.Bind (Get_Address_Member'Access);
--     Address_Mapping.Add_Mapping ("info", Property_Mapping'Access, Proxy_Info'Access);
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
   Person_Mapping.Add_Mapping ("address", Address_Mapping'Access, Proxy_Address'Access);
   Person_Mapping.Add_Default_Mapping;
   --  Person_Mapper.Add_Mapping ("info/*");
   --  Person_Mapper.Add_Mapping ("address/street/@number");

end Mapping;
