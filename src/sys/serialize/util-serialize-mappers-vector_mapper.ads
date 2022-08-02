-----------------------------------------------------------------------
--  Util.Serialize.Mappers.Vector_Mapper -- Mapper for vector types
--  Copyright (C) 2010, 2011, 2022 Stephane Carrez
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
with Ada.Containers;
with Ada.Containers.Vectors;
with Util.Serialize.Contexts;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.IO;
generic

   --  Package that represents the vectors of elements.
   with package Vectors is
     new Ada.Containers.Vectors (<>);

   --  Package that maps the element into a record.
   with package Element_Mapper is
     new Record_Mapper (Element_Type => Vectors.Element_Type,
                        others => <>);

package Util.Serialize.Mappers.Vector_Mapper is

   subtype Element_Type is Vectors.Element_Type;

   subtype Vector is Vectors.Vector;

   subtype Index_Type is Vectors.Index_Type;

   type Vector_Type_Access is access all Vector;

   --  Procedure to give access to the <b>Vector</b> object from the context.
   type Process_Object is not null
   access procedure (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                     Process : not null access procedure (Item : in out Vector));

   --  -----------------------
   --  Data context
   --  -----------------------
   --  Data context to get access to the target element.
   type Vector_Data is new Util.Serialize.Contexts.Data with private;
   type Vector_Data_Access is access all Vector_Data'Class;

   --  Get the vector object.
   function Get_Vector (Data : in Vector_Data) return Vector_Type_Access;

   --  Set the vector object.
   procedure Set_Vector (Data   : in out Vector_Data;
                         Vector : in Vector_Type_Access);

   --  -----------------------
   --  Record mapper
   --  -----------------------
   type Mapper is new Util.Serialize.Mappers.Mapper with private;
   type Mapper_Access is access all Mapper'Class;

   --  Execute the mapping operation on the object associated with the current context.
   --  The object is extracted from the context and the <b>Execute</b> operation is called.
   overriding
   procedure Execute (Handler : in Mapper;
                      Map     : in Mapping'Class;
                      Ctx     : in out Util.Serialize.Contexts.Context'Class;
                      Value   : in Util.Beans.Objects.Object);

   --  Set the <b>Data</b> vector in the context.
   procedure Set_Context (Ctx  : in out Util.Serialize.Contexts.Context'Class;
                          Data : in Vector_Type_Access);

   procedure Set_Mapping (Into  : in out Mapper;
                          Inner : in Element_Mapper.Mapper_Access);

   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   function Find_Mapper (Controller : in Mapper;
                         Name       : in String) return Util.Serialize.Mappers.Mapper_Access;

   overriding
   procedure Start_Object (Handler : in Mapper;
                           Context : in out Util.Serialize.Contexts.Context'Class;
                           Name    : in String);

   overriding
   procedure Finish_Object (Handler : in Mapper;
                            Context : in out Util.Serialize.Contexts.Context'Class;
                            Name    : in String);

   --  Write the element on the stream using the mapper description.
   procedure Write (Handler : in Mapper;
                    Stream  : in out Util.Serialize.IO.Output_Stream'Class;
                    Element : in Vectors.Vector);

private

   type Vector_Data is new Util.Serialize.Contexts.Data with record
      Vector   : Vector_Type_Access;
      Position : Index_Type;
   end record;

   type Mapper is new Util.Serialize.Mappers.Mapper with record
      Map         : aliased Element_Mapper.Mapper;
   end record;

   overriding
   procedure Initialize (Controller : in out Mapper);

end Util.Serialize.Mappers.Vector_Mapper;
