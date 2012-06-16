-----------------------------------------------------------------------
--  util-serialize-tools -- Tools to Serialize objects in various formats
--  Copyright (C) 2012 Stephane Carrez
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

with Util.Streams.Texts;
with Util.Streams.Buffered;
with Util.Serialize.IO.JSON;
with Util.Serialize.Mappers.Record_Mapper;
package body Util.Serialize.Tools is

   type Object_Field is (FIELD_NAME, FIELD_VALUE);
   type Object_Map_Access is access all Util.Beans.Objects.Maps.Map'Class;

   type Object_Mapper_Context is record
      Map  : Object_Map_Access;
      Name : Util.Beans.Objects.Object;
   end record;
   type Object_Mapper_Context_Access is access all Object_Mapper_Context;

   procedure Set_Member (Into  : in out Object_Mapper_Context;
                         Field : in Object_Field;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_NAME =>
            Into.Name := Value;

         when FIELD_VALUE =>
            Into.Map.Include (Util.Beans.Objects.To_String (Into.Name), Value);
            Into.Name := Util.Beans.Objects.Null_Object;

      end case;
   end Set_Member;

   package Object_Mapper is new
     Util.Serialize.Mappers.Record_Mapper (Element_Type        => Object_Mapper_Context,
                                           Element_Type_Access => Object_Mapper_Context_Access,
                                           Fields              => Object_Field,
                                           Set_Member          => Set_Member);

   Object_Mapping : aliased Object_Mapper.Mapper;

   procedure To_JSON (Output : in out Util.Serialize.IO.JSON.Output_Stream;
                     Map    : in Util.Beans.Objects.Maps.Map) is
      use type Ada.Containers.Count_Type;

      procedure Write (Name  : in String;
                       Value : in Util.Beans.Objects.Object) is
      begin
         Output.Start_Entity (Name => "");
         Output.Write_Attribute (Name  => "name",
                                 Value => Util.Beans.Objects.To_Object (Name));
         Output.Write_Attribute (Name  => "value",
                                 Value => Value);
         Output.End_Entity (Name => "");
      end Write;

   begin
      if Map.Length > 0 then
         declare
            Iter : Util.Beans.Objects.Maps.Cursor := Map.First;
         begin
            Output.Start_Array (Name   => "params",
                                Length => Map.Length);
            while Util.Beans.Objects.Maps.Has_Element (Iter) loop
               Util.Beans.Objects.Maps.Query_Element (Iter, Write'Access);
               Util.Beans.Objects.Maps.Next (Iter);
            end loop;
            Output.End_Array;
         end;
      end if;
   end To_JSON;

   --  -----------------------
   --  Serialize the objects defined in the object map <b>Map</b> into an XML stream.
   --  Returns the JSON string that contains a serialization of the object maps.
   --  -----------------------
   function To_JSON (Map : in Util.Beans.Objects.Maps.Map) return String is
      use type Ada.Containers.Count_Type;
   begin
      if Map.Length = 0 then
         return "";
      end if;
      declare
         Output : Util.Serialize.IO.JSON.Output_Stream;
      begin
         Output.Initialize (Size => 10000);
         Output.Start_Document;
         To_JSON (Output, Map);
         Output.End_Document;
         return Util.Streams.Texts.To_String (Util.Streams.Buffered.Buffered_Stream (Output));
      end;
   end To_JSON;

   --  -----------------------
   --  Deserializes the JSON content passed in <b>Content</b> and restore the object map
   --  which their values.
   --  Returns the object map that was restored.
   --  -----------------------
   function From_JSON (Content : in String) return Util.Beans.Objects.Maps.Map is
      Result  : aliased Util.Beans.Objects.Maps.Map;
      Parser  : Util.Serialize.IO.JSON.Parser;
      Context : aliased Object_Mapper_Context;
   begin
      if Content'Length > 0 then
         Context.Map := Result'Unchecked_Access;
         Parser.Add_Mapping ("/params", Object_Mapping'Access);
         Object_Mapper.Set_Context (Parser, Context'Unchecked_Access);
         Parser.Parse_String (Content);
      end if;
      return Result;
   end From_JSON;

begin
   Object_Mapping.Add_Mapping ("name", FIELD_NAME);
   Object_Mapping.Add_Mapping ("value", FIELD_VALUE);
end Util.Serialize.Tools;
