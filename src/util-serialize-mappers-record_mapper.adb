-----------------------------------------------------------------------
--  Util.Readers.Object_Reader -- Object Reader
--  Copyright (C) 2010 Stephane Carrez
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

   --  Data context to get access to the target element.
   type Element_Data is new Util.Serialize.Contexts.Data with record
      Element : Element_Type_Access;
   end record;
   type Element_Data_Access is access all Element_Data'Class;

   type Attribute_Mapping is new Mapping with record
      Index : Fields;
      Key   : Contexts.Data_Key;
   end record;
   type Attribute_Mapping_Access is access all Attribute_Mapping'Class;

   procedure Execute (Map   : in Attribute_Mapping;
                      Ctx   : in out Util.Serialize.Contexts.Context'Class;
                      Value : in Util.Beans.Objects.Object);

   --  Create a new object associated with the given name.
   --  The reader must be associated with the new object so
   --  that calls to <b>Set_Member</b> will apply on the new object.
--     procedure Create_Object (Parser : in out T_Reader;
--                              Name   : in String);

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
--     procedure Finish_Object (Parser : in out T_Reader;
--                              Name   : in String);

   --  Set the name/value pair on the current object.  The value is an
   --  object held by another parser.
--     procedure Set_Object (Parser : in out T_Reader;
--                           Name   : in String;
--                           Value  : in Reader'Class);

   procedure Add_Mapping (Into  : in out Mapper;
                          Path  : in String;
                          Field : in Fields) is
      Map : constant Attribute_Mapping_Access := new Attribute_Mapping;
   begin
      Map.Key   := Into.Key;
      Map.Index := Field;
--        Into.Add_Mapping (Path, Map);
   end Add_Mapping;
--
--     function Create_Context (Map : in T_Mapper) return Context_Access is
--        Result : Element_Context_Access := new Element_Context;
--     begin
--        return Result;
--     end Create_Context;

--     procedure Push_Context (Mapper : in Mapper;
--                             Context : in out Util.Serialize.Streamers.Reader'Class) is
--        --      Ctx :
--     begin
--        null;
--     end Push_Context;

   --  Execute the rule associated
   procedure Execute (Map   : in Attribute_Mapping;
                      Ctx   : in out Util.Serialize.Contexts.Context'Class;
                      Value : in Util.Beans.Objects.Object) is
      D : constant Contexts.Data_Access := Ctx.Get_Data (Map.Key);
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
         Set_Member (DE.Element.all, Map.Index, Value);
      end;
   end Execute;

end Util.Serialize.Mappers.Record_Mapper;