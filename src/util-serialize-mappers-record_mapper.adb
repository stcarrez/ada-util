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
with Util.Serialize.Contexts;

package body Util.Serialize.Mappers.Record_Mapper is

   Key : Util.Serialize.Contexts.Data_Key;

   --  -----------------------
   --  Get the element object.
   --  -----------------------
   function Get_Element (Data : in Element_Data) return Element_Type_Access is
   begin
      return Data.Element;
   end Get_Element;

   --  -----------------------
   --  Set the element object.
   --  -----------------------
   procedure Set_Element (Data    : in out Element_Data;
                          Element : in Element_Type_Access) is
   begin
      Data.Element := Element;
   end Set_Element;

   --  -----------------------
   --  Execute the process procedure on the object stored in the current data context.
   --  Raises No_Data if the context does not hold such data.
   --  -----------------------
   procedure Execute_Object (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                             Process : not null access procedure (Item : in out Element_Type)) is
      D : constant Contexts.Data_Access := Ctx.Get_Data (Key);
   begin
      if not (D.all in Element_Data'Class) then
         raise Util.Serialize.Contexts.No_Data;
      end if;
      declare
         DE : constant Element_Data_Access := Element_Data'Class (D.all)'Access;
      begin
         if DE.Element = null then
            raise Util.Serialize.Contexts.No_Data;
         end if;
         Process (DE.Element.all);
      end;
   end Execute_Object;

   --  -----------------------
   --  Add a mapping for setting a member.  When the attribute rule defined by <b>Path</b>
   --  is matched, the <b>Set_Member</b> procedure will be called with the value and the
   --  <b>Field</b> identification.
   --  -----------------------
   procedure Add_Mapping (Into  : in out Mapper;
                          Path  : in String;
                          Field : in Fields) is
      Map : constant Attribute_Mapping_Access := new Attribute_Mapping;
   begin
      Map.Index   := Field;
      Map.Process := Into.Process;
      Into.Add_Mapping (Path, Map.all'Access);
   end Add_Mapping;

   --  -----------------------
   --  Bind the mapper with the given process procedure.  The <b>Process</b> procedure is
   --  invoked to obtain the target element onto which the <b>Set_Member</b> procedure is called.
   --  The default process procedures obtains the target object from the data context.
   --  -----------------------
   procedure Bind (Into    : in out Mapper;
                   Process : in Process_Object) is
   begin
      Into.Process := Process;
   end Bind;

   --  -----------------------
   --  Execute the rule associated with the mapping.
   --  Set the data member associated with the mapping rule.
   --  -----------------------
   procedure Execute (Map   : in Attribute_Mapping;
                      Ctx   : in out Util.Serialize.Contexts.Context'Class;
                      Value : in Util.Beans.Objects.Object) is

      procedure Set_Member (P : in out Element_Type) is
      begin
         Set_Member (P, Map.Index, Value);
      end Set_Member;

   begin
      Map.Process (Ctx, Set_Member'Access);
   end Execute;

   --  -----------------------
   --  Set the element in the context.
   --  -----------------------
   procedure Set_Context (Ctx     : in out Util.Serialize.Contexts.Context'Class;
                          Element : in Element_Type_Access) is
      Data_Context : constant Element_Data_Access := new Element_Data;
   begin
      Data_Context.Element := Element;
      Ctx.Set_Data (Key => Key, Content => Data_Context.all'Access);
   end Set_Context;

   --  -----------------------
   --  Copy the mapping definitions defined by <b>From</b> into the target mapper
   --  and use the <b>Process</b> procedure to give access to the element.
   --  -----------------------
   procedure Copy (Into    : in out Mapper;
                   From    : in Mapper;
                   Process : in Process_Object) is
      Iter : Mapping_Map.Cursor := From.Rules.First;
   begin
      while Mapping_Map.Has_Element (Iter) loop
         declare
            Path : constant String := Mapping_Map.Key (Iter);
            E    : constant Mapping_Access := Mapping_Map.Element (Iter);
            Map  : constant Attribute_Mapping_Access := Attribute_Mapping'Class (E.all)'Access;
            N    : constant Attribute_Mapping_Access := new Attribute_Mapping;
         begin
            N.Index := Map.Index;
            N.Process := Process;
            Into.Add_Mapping (Path, N.all'Access);
         end;
         Mapping_Map.Next (Iter);
      end loop;
   end Copy;

begin
   --  Allocate the unique data key.
   Util.Serialize.Contexts.Allocate (Key);
end Util.Serialize.Mappers.Record_Mapper;