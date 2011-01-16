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
with Ada.Strings.Unbounded;

with Util.Serialize.Mappers;
package Mapping is

   use Ada.Strings.Unbounded;

   type Address is record
      City      : Unbounded_String;
      Street    : Unbounded_String;
      Country   : Unbounded_String;
      Zip       : Natural;
   end record;

   type Person is record
      First_Name : Unbounded_String;
      Last_Name  : Unbounded_String;
      Age        : Natural;
      Addr       : Address;
   end record;

   --  Get the address mapper which describes how to load an Address.
   function Get_Address_Mapper return Util.Serialize.Mappers.Mapper_Access;

   --  Get the person mapper which describes how to load a Person.
   function Get_Person_Mapper return Util.Serialize.Mappers.Mapper_Access;

end Mapping;
