-----------------------------------------------------------------------
--  Util.Serialize.Mappers.Record_Mapper -- Mapper for record types
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
generic
   type Element_Type is limited private;
   type Element_Type_Access is access all Element_Type;
   type Fields is (<>);
   with procedure Set_Member (Into   : in out Element_Type;
                              Field  : in Fields;
                              Value  : in Util.Beans.Objects.Object);
package Util.Serialize.Mappers.Record_Mapper is

   --  Procedure to give access to the <b>Element_Type</b> object from the context.
   type Process_Object is not null
   access procedure (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                     Process : not null access procedure (Item : in out Element_Type));

   --  -----------------------
   --  Data context
   --  -----------------------
   --  Data context to get access to the target element.
   type Element_Data is new Util.Serialize.Contexts.Data with private;
   type Element_Data_Access is access all Element_Data'Class;

   --  Get the element object.
   function Get_Element (Data : in Element_Data) return Element_Type_Access;

   --  Set the element object.
   procedure Set_Element (Data    : in out Element_Data;
                          Element : in Element_Type_Access);

   --  Execute the process procedure on the object stored in the current data context.
   --  Raises No_Data if the context does not hold such data.
   procedure Execute_Object (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                             Process : not null access procedure (Item : in out Element_Type));

   --  -----------------------
   --  Record mapper
   --  -----------------------
   type Mapper is new Util.Serialize.Mappers.Mapper with private;
   type Mapper_Access is access all Mapper'Class;

   --  Add a mapping for setting a member.  When the attribute rule defined by <b>Path</b>
   --  is matched, the <b>Set_Member</b> procedure will be called with the value and the
   --  <b>Field</b> identification.
   procedure Add_Mapping (Into  : in out Mapper;
                          Path  : in String;
                          Field : in Fields);

   --  Bind the mapper with the given process procedure.  The <b>Process</b> procedure is
   --  invoked to obtain the target element onto which the <b>Set_Member</b> procedure is called.
   --  The default process procedures obtains the target object from the data context.
   procedure Bind (Into    : in out Mapper;
                   Process : in Process_Object);

   --  Set the element in the context.
   procedure Set_Context (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                          Element : in Element_Type_Access);

   --  Copy the mapping definitions defined by <b>From</b> into the target mapper
   --  and use the <b>Process</b> procedure to give access to the element.
   procedure Copy (Into    : in out Mapper;
                   From    : in Mapper;
                   Process : in Process_Object);

private

   type Element_Data is new Util.Serialize.Contexts.Data with record
      Element : Element_Type_Access;
   end record;

   type Mapper is new Util.Serialize.Mappers.Mapper with record
      Process : Process_Object := Execute_Object'Access;
   end record;

   type Attribute_Mapping is new Mapping with record
      Index   : Fields;
      Process : Process_Object := Execute_Object'Access;
   end record;
   type Attribute_Mapping_Access is access all Attribute_Mapping'Class;

   procedure Execute (Map   : in Attribute_Mapping;
                      Ctx   : in out Util.Serialize.Contexts.Context'Class;
                      Value : in Util.Beans.Objects.Object);

end Util.Serialize.Mappers.Record_Mapper;