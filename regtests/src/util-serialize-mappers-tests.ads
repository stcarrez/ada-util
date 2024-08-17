-----------------------------------------------------------------------
--  serialize-mappers-tests -- Unit tests for serialization
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
