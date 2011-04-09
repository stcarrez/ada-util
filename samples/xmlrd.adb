-----------------------------------------------------------------------
--  json -- JSON Reader
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
with Ada.Text_IO;
with Ada.Command_Line;
with Util.Serialize.IO.JSON;
with Ada.Containers;
with Ada.Containers.Vectors;
with Mapping;

with Util.Beans;
with Util.Beans.Objects;
with Util.Beans.Objects.Vectors;
with Util.Streams.Texts;
with Util.Streams.Buffered;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.Mappers.Vector_Mapper;
with Util.Serialize.IO.XML;
procedure Xmlrd is

   use Util.Streams.Buffered;
   use Ada.Strings.Unbounded;
   use type Mapping.Person_Access;
   use type Ada.Containers.Count_Type;

   Reader : Util.Serialize.IO.XML.Parser;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;

   type Rule_Fields is (FIELD_ROLE, FIELD_URL_PATTERN);

   type Rule is record
      Roles    : Util.Beans.Objects.Vectors.Vector;
      Patterns : Util.Beans.Objects.Vectors.Vector;
   end record;
   type Rule_Access is access all Rule;

   package Rule_Vector is
     new Ada.Containers.Vectors (Element_Type => Rule,
                                 Index_Type   => Natural);

   procedure Set_Member (P     : in out Rule;
                         Field : in Rule_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_ROLE =>
            P.Roles.Append (Value);

         when FIELD_URL_PATTERN =>
            P.Patterns.Append (Value);

      end case;
   end Set_Member;

   package Rule_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Rule,
                                               Element_Type_Access => Rule_Access,
                                               Fields              => Rule_Fields,
                                               Set_Member          => Set_Member);

   package Rule_Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (Vectors        => Rule_Vector,
                                               Element_Mapper => Rule_Mapper);

   procedure Print (P : in Rule) is
   begin
      Ada.Text_IO.Put_Line ("rule : ");
   end Print;

   procedure Print (P : in Rule_Vector.Cursor) is
   begin
      Print (Rule_Vector.Element (P));
   end Print;

   Rule_Mapping        : aliased Rule_Mapper.Mapper;
   Rule_Vector_Mapping : aliased Rule_Vector_Mapper.Mapper;

begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: xmlrd file...");
      return;
   end if;

   Rule_Mapping.Add_Mapping ("role", FIELD_ROLE);
   Rule_Mapping.Add_Mapping ("url-pattern", FIELD_URL_PATTERN);
   Rule_Vector_Mapping.Set_Mapping ("", Rule_Mapping'Unchecked_Access);

   Reader.Add_Mapping ("access-rules/rule", Rule_Vector_Mapping'Unchecked_Access);

   for I in 1 .. Count loop
      declare
         S    : constant String := Ada.Command_Line.Argument (I);

         List : aliased Rule_Vector_Mapper.Vector;
      begin
         Rule_Vector_Mapper.Set_Context (Reader, List'Unchecked_Access);
         Reader.Parse (S);

         --  The list now contains our elements.
         List.Iterate (Process => Print'Access);
--           if List.Length = 0 then
--              Print (P);
--           end if;
--
--           declare
--              Output : Util.Serialize.IO.JSON.Output_Stream;
--           begin
--              Output.Initialize (Size => 10000);
--              Mapping.Get_Person_Mapper.Write (Output, P);
--              Ada.Text_IO.Put_Line ("Person: " & Util.Streams.Texts.To_String (Buffered_Stream (Output)));
--           end;

--           declare
--              Output : Util.Serialize.IO.JSON.Output_Stream;
--           begin
--              Output.Initialize (Size => 10000);
--              Output.Write ("{""list"":");
--              Mapping.Get_Person_Vector_Mapper.Write (Output, List);
--              Output.Write ("}");
--
--              Ada.Text_IO.Put_Line ("IO:");
--              Ada.Text_IO.Put_Line (Util.Streams.Texts.To_String (Buffered_Stream (Output)));
--           end;
      end;
   end loop;
end Xmlrd;
