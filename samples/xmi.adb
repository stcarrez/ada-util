-----------------------------------------------------------------------
--  xmi -- XMI parser example
--  Copyright (C) 2012, 2022 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Command_Line;

with Util.Beans;
with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.IO.XML;

--  This sample reads an UML 1.4 model saved in XMI 1.2 format.  It uses the serialization
--  framework to indicate what XML nodes and attributes must be reported.
procedure XMI is

   use type Ada.Text_IO.Count;

   Reader : Util.Serialize.IO.XML.Parser;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;

   type XMI_Fields is (FIELD_CLASS_NAME, FIELD_CLASS_ID, FIELD_STEREOTYPE,
                       FIELD_ATTRIBUTE_NAME,
                       FIELD_PACKAGE_NAME,
                       FIELD_PACKAGE_END,
                       FIELD_VISIBILITY,
                       FIELD_DATA_TYPE,
                       FIELD_ENUM_DATA_TYPE,
                       FIELD_ENUM_NAME,
                       FIELD_ENUM_END,
                       FIELD_ENUM_LITERAL,
                       FIELD_ENUM_LITERAL_END,
                       FIELD_CLASS_END,
                       FIELD_MULTIPLICITY_LOWER,
                       FIELD_MULTIPLICITY_UPPER,
                       FIELD_ASSOCIATION_AGGREGATION,
                       FIELD_ASSOCIATION_NAME,
                       FIELD_ASSOCIATION_VISIBILITY,
                       FIELD_ASSOCIATION_END_ID,
                       FIELD_ASSOCIATION_END,
                       FIELD_TAG_DEFINITION_ID,
                       FIELD_TAG_DEFINITION_NAME,
                       FIELD_OPERATION_NAME,
                       FIELD_COMMENT_NAME,
                       FIELD_COMMENT_BODY,
                       FIELD_COMMENT_CLASS_ID,
                       FIELD_TAGGED_VALUE_TYPE,
                       FIELD_TAGGED_VALUE_VALUE);

   type XMI_Info is record
      Indent : Ada.Text_IO.Count := 1;
   end record;
   type XMI_Access is access all XMI_Info;

   procedure Set_Member (P     : in out XMI_Info;
                         Field : in XMI_Fields;
                         Value : in Util.Beans.Objects.Object);
   procedure Print (Col  : in Ada.Text_IO.Count;
                    Line : in String);

   procedure Print (Col  : in Ada.Text_IO.Count;
                    Line : in String) is
   begin
      Ada.Text_IO.Set_Col (Col);
      Ada.Text_IO.Put_Line (Line);
   end Print;

   procedure Set_Member (P     : in out XMI_Info;
                         Field : in XMI_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
      when FIELD_CLASS_NAME =>
         Print (P.Indent, "Class " & Util.Beans.Objects.To_String (Value));
         P.Indent := P.Indent + 2;

      when FIELD_VISIBILITY =>
         Print (P.Indent, "visibility: " & Util.Beans.Objects.To_String (Value));

      when FIELD_CLASS_ID =>
         Print (P.Indent, "id: " & Util.Beans.Objects.To_String (Value));

      when FIELD_ATTRIBUTE_NAME =>
         Ada.Text_IO.Set_Col (P.Indent);
         Ada.Text_IO.Put ("attr: " & Util.Beans.Objects.To_String (Value));

      when FIELD_MULTIPLICITY_LOWER =>
         Ada.Text_IO.Set_Col (P.Indent);
         Ada.Text_IO.Put ("  multiplicity: " & Util.Beans.Objects.To_String (Value));

      when FIELD_MULTIPLICITY_UPPER =>
         Ada.Text_IO.Put_Line (".." & Util.Beans.Objects.To_String (Value));

      when FIELD_STEREOTYPE =>
         Print (P.Indent, "<<" & Util.Beans.Objects.To_String (Value) & ">>");

      when FIELD_DATA_TYPE =>
         Print (P.Indent, "  data-type:" & Util.Beans.Objects.To_String (Value));

      when FIELD_ENUM_DATA_TYPE =>
         Print (P.Indent, "  enum-type:" & Util.Beans.Objects.To_String (Value));
         P.Indent := P.Indent + 2;

      when FIELD_ENUM_NAME =>
         Print (P.Indent, "  enum " & Util.Beans.Objects.To_String (Value));
         P.Indent := P.Indent + 2;

      when FIELD_ENUM_LITERAL =>
         Print (P.Indent, "  enum:" & Util.Beans.Objects.To_String (Value));
         P.Indent := P.Indent + 2;

      when FIELD_OPERATION_NAME =>
         Print (P.Indent, "operation:" & Util.Beans.Objects.To_String (Value));

      when FIELD_ASSOCIATION_NAME =>
         Print (P.Indent, "association " & Util.Beans.Objects.To_String (Value));
         P.Indent := P.Indent + 2;

      when FIELD_ASSOCIATION_VISIBILITY =>
         Print (P.Indent, "visibility: " & Util.Beans.Objects.To_String (Value));

      when FIELD_ASSOCIATION_AGGREGATION =>
         Print (P.Indent, "   aggregate: " & Util.Beans.Objects.To_String (Value));

      when FIELD_ASSOCIATION_END_ID =>
         Print (P.Indent, "   end-id: " & Util.Beans.Objects.To_String (Value));

      when FIELD_PACKAGE_NAME =>
         Print (P.Indent, "package " & Util.Beans.Objects.To_String (Value));
         P.Indent := P.Indent + 2;

      when FIELD_TAGGED_VALUE_VALUE =>
         Print (P.Indent, "-- " & Util.Beans.Objects.To_String (Value));

      when FIELD_TAGGED_VALUE_TYPE =>
         Print (P.Indent, "tag-type: " & Util.Beans.Objects.To_String (Value));

      when FIELD_TAG_DEFINITION_NAME =>
         Print (P.Indent, "Tag: " & Util.Beans.Objects.To_String (Value));

      when FIELD_TAG_DEFINITION_ID =>
         Print (P.Indent, "  tag-id: " & Util.Beans.Objects.To_String (Value));

      when FIELD_COMMENT_NAME =>
         Print (P.Indent, "Comment: " & Util.Beans.Objects.To_String (Value));

      when FIELD_COMMENT_BODY =>
         Print (P.Indent, "   text: " & Util.Beans.Objects.To_String (Value));

      when FIELD_COMMENT_CLASS_ID =>
         Print (P.Indent, "   for-id: " & Util.Beans.Objects.To_String (Value));

      when FIELD_PACKAGE_END | FIELD_CLASS_END | FIELD_ENUM_END | FIELD_ENUM_LITERAL_END |
           FIELD_ASSOCIATION_END =>
         P.Indent := P.Indent - 2;

      end case;
   end Set_Member;

   package XMI_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => XMI_Info,
                                               Element_Type_Access => XMI_Access,
                                               Fields              => XMI_Fields,
                                               Set_Member          => Set_Member);

   XMI_Mapping        : aliased XMI_Mapper.Mapper;
   Mapper             : Util.Serialize.Mappers.Processing;

begin
--        Util.Log.Loggers.Initialize ("samples/log4j.properties");

   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: xmi file...");
      Ada.Text_IO.Put_Line ("Example xmi samples/demo-atlas.xmi");
      return;
   end if;

   --  Define the XMI mapping.
   XMI_Mapping.Add_Mapping ("**/Package/@name", FIELD_PACKAGE_NAME);
   XMI_Mapping.Add_Mapping ("**/Package/@visibility", FIELD_VISIBILITY);
   XMI_Mapping.Add_Mapping ("**/Package", FIELD_PACKAGE_END);

   XMI_Mapping.Add_Mapping ("**/Class/@name", FIELD_CLASS_NAME);
   XMI_Mapping.Add_Mapping ("**/Class/@xmi.idref", FIELD_CLASS_NAME);
   XMI_Mapping.Add_Mapping ("**/Class/@xmi.id", FIELD_CLASS_ID);
   XMI_Mapping.Add_Mapping ("**/Class/@visibility", FIELD_VISIBILITY);
   XMI_Mapping.Add_Mapping ("**/Class", FIELD_CLASS_END);

   XMI_Mapping.Add_Mapping ("**/Stereotype/@href", FIELD_STEREOTYPE);
   XMI_Mapping.Add_Mapping ("**/Stereotype/@name", FIELD_STEREOTYPE);

   XMI_Mapping.Add_Mapping ("**/Attribute/@name", FIELD_ATTRIBUTE_NAME);
   XMI_Mapping.Add_Mapping ("**/Attribute/@visibility", FIELD_VISIBILITY);

   XMI_Mapping.Add_Mapping ("**/TaggedValue.dataValue", FIELD_TAGGED_VALUE_VALUE);

   XMI_Mapping.Add_Mapping ("**/DataType/@href", FIELD_DATA_TYPE);

   XMI_Mapping.Add_Mapping ("**/Enumeration/@href", FIELD_ENUM_NAME);
   XMI_Mapping.Add_Mapping ("**/Enumeration/@name", FIELD_ENUM_NAME);
   XMI_Mapping.Add_Mapping ("**/Enumeration", FIELD_ENUM_END);

   XMI_Mapping.Add_Mapping ("**/EnumerationLiteral/@name", FIELD_ENUM_LITERAL);
   XMI_Mapping.Add_Mapping ("**/EnumerationLiteral", FIELD_ENUM_LITERAL_END);

   XMI_Mapping.Add_Mapping ("**/MultiplicityRange/@lower", FIELD_MULTIPLICITY_LOWER);
   XMI_Mapping.Add_Mapping ("**/MultiplicityRange/@upper", FIELD_MULTIPLICITY_UPPER);

   XMI_Mapping.Add_Mapping ("**/Operation/@name", FIELD_OPERATION_NAME);
   XMI_Mapping.Add_Mapping ("**/Operation/@visibility", FIELD_VISIBILITY);

   XMI_Mapping.Add_Mapping ("**/Association/@name", FIELD_ASSOCIATION_NAME);
   XMI_Mapping.Add_Mapping ("**/Association/@visibility", FIELD_ASSOCIATION_VISIBILITY);
   XMI_Mapping.Add_Mapping ("**/Association/@aggregation", FIELD_ASSOCIATION_AGGREGATION);
   XMI_Mapping.Add_Mapping ("**/AssociationEnd.participant/Class/@xmi.idref",
                            FIELD_ASSOCIATION_END_ID);
   XMI_Mapping.Add_Mapping ("**/Association", FIELD_ASSOCIATION_END);

   XMI_Mapping.Add_Mapping ("**/TagDefinition/@name", FIELD_TAG_DEFINITION_NAME);
   XMI_Mapping.Add_Mapping ("**/TagDefinition/@xm.id", FIELD_TAG_DEFINITION_ID);

   XMI_Mapping.Add_Mapping ("**/Comment/@name", FIELD_COMMENT_NAME);
   XMI_Mapping.Add_Mapping ("**/Comment/@body", FIELD_COMMENT_BODY);
   XMI_Mapping.Add_Mapping ("**/Comment/Comment.annotatedElement/Class/@xmi.idref",
                            FIELD_COMMENT_CLASS_ID);

   Mapper.Add_Mapping ("XMI", XMI_Mapping'Unchecked_Access);

   for I in 1 .. Count loop
      declare
         S    : constant String := Ada.Command_Line.Argument (I);
         Data : aliased XMI_Info;
      begin
         XMI_Mapper.Set_Context (Mapper, Data'Unchecked_Access);
         Reader.Parse (S, Mapper);
      end;
   end loop;
end XMI;
