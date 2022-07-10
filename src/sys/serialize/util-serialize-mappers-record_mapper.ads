-----------------------------------------------------------------------
--  Util.Serialize.Mappers.Record_Mapper -- Mapper for record types
--  Copyright (C) 2010, 2011, 2012, 2022 Stephane Carrez
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
with Util.Serialize.IO;
generic
   type Element_Type (<>) is limited private;
   type Element_Type_Access is access all Element_Type;
   type Fields is (<>);

   --  The <b>Set_Member</b> procedure will be called by the mapper when a mapping associated
   --  with <b>Field</b> is recognized.  The <b>Value</b> holds the value that was extracted
   --  according to the mapping.  The <b>Set_Member</b> procedure should save in the target
   --  object <b>Into</b> the value.  If an error is detected, the procedure can raise the
   --  <b>Util.Serialize.Mappers.Field_Error</b> exception.  The exception message will be
   --  reported by the IO reader as an error.
   with procedure Set_Member (Into   : in out Element_Type;
                              Field  : in Fields;
                              Value  : in Util.Beans.Objects.Object);

   --  Adding a second function/procedure as generic parameter makes the
   --  Vector_Mapper generic package fail to instantiate a valid package.
   --  The failure occurs in Vector_Mapper in the 'with package Element_Mapper' instantiation.
   --
--        with function Get_Member (From  : in Element_Type;
--                                  Field : in Fields) return Util.Beans.Objects.Object;
package Util.Serialize.Mappers.Record_Mapper is

   type Get_Member_Access is
     access function (From  : in Element_Type;
                      Field : in Fields) return Util.Beans.Objects.Object;

   --  Procedure to give access to the <b>Element_Type</b> object from the context.
   type Process_Object is not null
   access procedure (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                     Attr    : in Mapping'Class;
                     Value   : in Util.Beans.Objects.Object;
                     Process : not null
                     access procedure (Attr  : in Mapping'Class;
                                       Item  : in out Element_Type;
                                       Value : in Util.Beans.Objects.Object));

   type Proxy_Object is not null
   access procedure (Attr    : in Mapping'Class;
                     Element : in out Element_Type;
                     Value   : in Util.Beans.Objects.Object);

   --  Set the attribute member described by the <b>Attr</b> mapping
   --  into the value passed in <b>Element</b>.  This operation will call
   --  the package parameter function of the same name.
   procedure Set_Member (Attr    : in Mapping'Class;
                         Element : in out Element_Type;
                         Value   : in Util.Beans.Objects.Object);

   --  -----------------------
   --  Data context
   --  -----------------------
   --  Data context to get access to the target element.
   type Element_Data is new Util.Serialize.Contexts.Data with private;
   type Element_Data_Access is access all Element_Data'Class;

   --  Get the element object.
   function Get_Element (Data : in Element_Data) return Element_Type_Access;

   --  Set the element object.  When <b>Release</b> is set, the element <b>Element</b>
   --  will be freed when the reader context is deleted (by <b>Finalize</b>).
   procedure Set_Element (Data    : in out Element_Data;
                          Element : in Element_Type_Access;
                          Release : in Boolean := False);

   --  Finalize the object when it is removed from the reader context.
   --  If the <b>Release</b> parameter was set, the target element will be freed.
   overriding
   procedure Finalize (Data : in out Element_Data);

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

   --  Add a mapping for setting a member.  When the attribute rule defined by <b>Path</b>
   --  is matched, the <b>Set_Member</b> procedure will be called with the value and the
   --  <b>Field</b> identification.
   procedure Add_Mapping (Into  : in out Mapper;
                          Path  : in String;
                          Field : in Fields);

   --  Add a mapping associated with the path and described by a mapper object.
   --  The <b>Proxy</b> procedure is in charge of giving access to the target
   --  object used by the <b>Map</b> mapper.
   procedure Add_Mapping (Into  : in out Mapper;
                          Path  : in String;
                          Map   : in Util.Serialize.Mappers.Mapper_Access;
                          Proxy : in Proxy_Object);

   --  Clone the <b>Handler</b> instance and get a copy of that single object.
   overriding
   function Clone (Handler : in Mapper) return Util.Serialize.Mappers.Mapper_Access;

   --
   procedure Bind (Into    : in out Mapper;
                   Getter  : in Get_Member_Access);

   procedure Bind (Into    : in out Mapper;
                   From    : in Mapper_Access);

   function Get_Getter (From : in Mapper) return Get_Member_Access;

   --  Set the element in the context.  When <b>Release</b> is set, the element <b>Element</b>
   --  will be freed when the reader context is deleted (by <b>Finalize</b>).
   procedure Set_Context (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                          Element : in Element_Type_Access;
                          Release : in Boolean := False);

   --  Build a default mapping based on the <b>Fields</b> enumeration.
   --  The enumeration name is used for the mapping name with the optional <b>FIELD_</b>
   --  prefix stripped.
   procedure Add_Default_Mapping (Into : in out Mapper);

   --  Write the element on the stream using the mapper description.
   procedure Write (Handler : in Mapper;
                    Stream  : in out Util.Serialize.IO.Output_Stream'Class;
                    Element : in Element_Type);

   --  Write the element on the stream using the mapper description.
   procedure Write (Handler : in Util.Serialize.Mappers.Mapper'Class;
                    Getter  : in Get_Member_Access;
                    Stream  : in out Util.Serialize.IO.Output_Stream'Class;
                    Element : in Element_Type);

private

   type Element_Data is new Util.Serialize.Contexts.Data with record
      Element : Element_Type_Access;
      Release : Boolean;
   end record;

   type Mapper is new Util.Serialize.Mappers.Mapper with record
      Execute    : Proxy_Object := Set_Member'Access;
      Get_Member : Get_Member_Access;
   end record;

   type Attribute_Mapping is new Mapping with record
      Index   : Fields;
   end record;
   type Attribute_Mapping_Access is access all Attribute_Mapping'Class;

   procedure Set_Member (Attr    : in Attribute_Mapping;
                         Element : in out Element_Type;
                         Value   : in Util.Beans.Objects.Object);

   type Proxy_Mapper is new Mapper with null record;
   type Proxy_Mapper_Access is access all Proxy_Mapper'Class;

   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   overriding
   function Find_Mapper (Controller : in Proxy_Mapper;
                         Name       : in String;
                         Attribute  : in Boolean := False)
                         return Util.Serialize.Mappers.Mapper_Access;

end Util.Serialize.Mappers.Record_Mapper;
