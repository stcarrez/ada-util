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
with Ada.Strings.Unbounded;

with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.Mappers;
with Ada.Containers.Vectors;
package Mapping is

   use Ada.Strings.Unbounded;

   type Property is record
      Name     : Unbounded_String;
      Value    : Unbounded_String;
   end record;

   type Address is record
      City      : Unbounded_String;
      Street    : Unbounded_String;
      Country   : Unbounded_String;
      Zip       : Natural := 0;
      Info      : Property;
   end record;

   type Person is record
      Name       : Unbounded_String;
      First_Name : Unbounded_String;
      Last_Name  : Unbounded_String;
      Username   : Unbounded_String;
      Gender     : Unbounded_String;
      Link       : Unbounded_String;
      Age        : Natural := 0;
      Addr       : Address;
      Id         : Long_Long_Integer := 0;
   end record;

   type Person_Access is access all Person;

   type Person_Fields is (FIELD_FIRST_NAME, FIELD_LAST_NAME, FIELD_AGE,
                          FIELD_NAME, FIELD_USER_NAME, FIELD_GENDER,
                          FIELD_LINK, FIELD_ID);

   --  Set the name/value pair on the current object.
   procedure Set_Member (P     : in out Person;
                         Field : in Person_Fields;
                         Value : in Util.Beans.Objects.Object);

   function Get_Person_Member (From  : in Person;
                               Field : in Person_Fields) return Util.Beans.Objects.Object;

   package Person_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Person,
                                               Element_Type_Access => Person_Access,
                                               Fields              => Person_Fields,
                                               Set_Member          => Set_Member);

   subtype Person_Context is Person_Mapper.Element_Data;

   package Person_Vector is
     new Ada.Containers.Vectors (Element_Type => Person,
                                 Index_Type   => Natural);

   --  Get the address mapper which describes how to load an Address.
   function Get_Address_Mapper return Util.Serialize.Mappers.Mapper_Access;

   --  Get the person mapper which describes how to load a Person.
   function Get_Person_Mapper return Person_Mapper.Mapper_Access;

end Mapping;
