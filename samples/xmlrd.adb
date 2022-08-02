-----------------------------------------------------------------------
--  xrds -- XRDS parser example
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
with Ada.Containers;
with Ada.Containers.Vectors;

with Util.Log.Loggers;
with Util.Beans;
with Util.Beans.Objects;
with Util.Streams.Texts;
with Util.Streams.Buffered;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.Mappers.Vector_Mapper;
with Util.Serialize.IO.XML;
procedure Xmlrd is

   use Ada.Containers;
   use Util.Streams.Buffered;
   use Ada.Strings.Unbounded;

   Reader : Util.Serialize.IO.XML.Parser;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;

   type Service_Fields is (FIELD_PRIORITY, FIELD_TYPE, FIELD_URI, FIELD_LOCAL_ID, FIELD_DELEGATE);

   type Service is record
      Priority : Integer;
      URI      : Ada.Strings.Unbounded.Unbounded_String;
      RDS_Type : Ada.Strings.Unbounded.Unbounded_String;
      Delegate : Ada.Strings.Unbounded.Unbounded_String;
      Local_Id : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Service_Access is access all Service;

   package Service_Vector is
     new Ada.Containers.Vectors (Element_Type => Service,
                                 Index_Type   => Natural);

   procedure Print (P : in Service_Vector.Cursor);
   procedure Print (P : in Service);
   procedure Set_Member (P     : in out Service;
                         Field : in Service_Fields;
                         Value : in Util.Beans.Objects.Object);
   function Get_Member (P     : in Service;
                        Field : in Service_Fields) return Util.Beans.Objects.Object;

   procedure Set_Member (P     : in out Service;
                         Field : in Service_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      Ada.Text_IO.Put_Line ("Set member " & Service_Fields'Image (Field)
                            & " to " & Util.Beans.Objects.To_String (Value));
      case Field is
         when FIELD_PRIORITY =>
            P.Priority := Util.Beans.Objects.To_Integer (Value);

         when FIELD_TYPE =>
            P.RDS_Type := Util.Beans.Objects.To_Unbounded_String (Value);

         when FIELD_URI =>
            P.URI := Util.Beans.Objects.To_Unbounded_String (Value);

         when FIELD_LOCAL_ID =>
            P.Local_Id := Util.Beans.Objects.To_Unbounded_String (Value);

         when FIELD_DELEGATE =>
            P.Delegate := Util.Beans.Objects.To_Unbounded_String (Value);

      end case;
   end Set_Member;

   function Get_Member (P : in Service;
                        Field : in Service_Fields) return Util.Beans.Objects.Object is
   begin
      case Field is
         when FIELD_PRIORITY =>
            return Util.Beans.Objects.To_Object (P.Priority);

         when FIELD_TYPE =>
            return Util.Beans.Objects.To_Object (P.RDS_Type);

         when FIELD_URI =>
            return Util.Beans.Objects.To_Object (P.URI);

         when FIELD_LOCAL_ID =>
            return Util.Beans.Objects.To_Object (P.Local_Id);

         when FIELD_DELEGATE =>
            return Util.Beans.Objects.To_Object (P.Delegate);

      end case;
   end Get_Member;

   package Service_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Service,
                                               Element_Type_Access => Service_Access,
                                               Fields              => Service_Fields,
                                               Set_Member          => Set_Member);

   package Service_Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (Vectors        => Service_Vector,
                                               Element_Mapper => Service_Mapper);


   procedure Print (P : in Service) is
   begin
      Ada.Text_IO.Put_Line ("        URI: " & To_String (P.URI));
      Ada.Text_IO.Put_Line ("   Priority: " & Integer'Image (P.Priority));
      Ada.Text_IO.Put_Line ("Type (last): " & To_String (P.RDS_Type));
      Ada.Text_IO.Put_Line ("   Delegate: " & To_String (P.Delegate));
      Ada.Text_IO.New_Line;
   end Print;

   procedure Print (P : in Service_Vector.Cursor) is
   begin
      Print (Service_Vector.Element (P));
   end Print;

   Service_Mapping        : aliased Service_Mapper.Mapper;
   Service_Vector_Mapping : aliased Service_Vector_Mapper.Mapper;
   Mapper                 : Util.Serialize.Mappers.Processing;
begin
   Util.Log.Loggers.Initialize ("samples/log4j.properties");

   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: xmlrd file...");
      return;
   end if;

   Service_Mapping.Add_Mapping ("Type", FIELD_TYPE);
   Service_Mapping.Add_Mapping ("URI", FIELD_URI);
   Service_Mapping.Add_Mapping ("Delegate", FIELD_DELEGATE);
   Service_Mapping.Add_Mapping ("@priority", FIELD_PRIORITY);
   Service_Mapping.Bind (Get_Member'Access);
   Service_Vector_Mapping.Set_Mapping (Service_Mapping'Unchecked_Access);

   Mapper.Add_Mapping ("XRDS/XRD/Service", Service_Vector_Mapping'Unchecked_Access);

   for I in 1 .. Count loop
      declare
         S    : constant String := Ada.Command_Line.Argument (I);

         List : aliased Service_Vector_Mapper.Vector;
      begin
         Service_Vector_Mapper.Set_Context (Mapper, List'Unchecked_Access);
         Reader.Parse (S, Mapper);

         Ada.Text_IO.Put_Line ("Rule count: " & Count_Type'Image (List.Length));

         --  The list now contains our elements.
         List.Iterate (Process => Print'Access);

         declare
            Buffer : aliased Util.Streams.Texts.Print_Stream;
            Output : Util.Serialize.IO.XML.Output_Stream;
         begin
            Buffer.Initialize (Size => 10000);
            Output.Initialize (Output => Buffer'Unchecked_access);
            Output.Start_Entity ("XRDS");
            Service_Vector_Mapping.Write (Output, List);
            Output.End_Entity ("XRDS");
            Ada.Text_IO.Put_Line (Util.Streams.Texts.To_String (Buffer));
         end;
      end;
   end loop;
end Xmlrd;
