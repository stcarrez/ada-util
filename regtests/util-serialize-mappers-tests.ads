-----------------------------------------------------------------------
--  serialize-mappers-tests -- Unit tests for serialization
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
with Ada.Containers.Vectors;

with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.Mappers.Vector_Mapper;
package Util.Serialize.Mappers.Tests is

   use Ada.Strings.Unbounded;

   type Map_Test is record
      Value : Natural;
      Bool  : Boolean;
      Name  : Unbounded_String;
      Node  : Util.Beans.Objects.Object;
   end record;
   type Map_Test_Access is access all Map_Test;

   type Map_Test_Fields is (FIELD_VALUE, FIELD_BOOL, FIELD_NAME, FIELD_NODE);


   procedure Set_Member (P     : in out Map_Test;
                         Field : in Map_Test_Fields;
                         Value : in Util.Beans.Objects.Object);

   function Get_Member (P : in Map_Test;
                        Field : in Map_Test_Fields) return Util.Beans.Objects.Object;

   package Map_Test_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Map_Test,
                                               Element_Type_Access => Map_Test_Access,
                                               Fields              => Map_Test_Fields,
                                               Set_Member          => Set_Member);

   package Map_Test_Vector is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Map_Test);

   package Map_Test_Vector_Mapper is
      new Util.Serialize.Mappers.Vector_Mapper (Vectors        => Map_Test_Vector,
                                                Element_Mapper => Map_Test_Mapper);

end Util.Serialize.Mappers.Tests;
