-----------------------------------------------------------------------
--  json -- JSON Reader
--  Copyright (C) 2010, 2011, 2014, 2017, 2022 Stephane Carrez
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
with Mapping;
with Util.Serialize.Mappers.Vector_Mapper;
with Util.Streams.Texts;
with Util.Streams.Buffered;
procedure Json is

   use Util.Streams.Buffered;
   use Ada.Strings.Unbounded;
   use type Ada.Containers.Count_Type;
   use Mapping;

   Reader : Util.Serialize.IO.JSON.Parser;
   Mapper : Util.Serialize.Mappers.Processing;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;

   package Person_Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (Vectors        => Person_Vector,
                                               Element_Mapper => Person_Mapper);

   --  Mapping for a list of Person records (stored as a Vector).
   Person_Vector_Mapping : aliased Person_Vector_Mapper.Mapper;

   procedure Print (P : in Mapping.Person_Vector.Cursor);
   procedure Print (P : in Mapping.Person);

   procedure Print (P : in Mapping.Person) is
   begin
      Ada.Text_IO.Put_Line ("Name       : " & To_String (P.Name));
      Ada.Text_IO.Put_Line ("first_name : " & To_String (P.First_Name));
      Ada.Text_IO.Put_Line ("last_name  : " & To_String (P.Last_Name));
      Ada.Text_IO.Put_Line ("Age        : " & Natural'Image (P.Age));
      Ada.Text_IO.Put_Line ("Street     : " & To_String (P.Addr.Street));
      Ada.Text_IO.Put_Line ("City       : " & To_String (P.Addr.City));
      Ada.Text_IO.Put_Line ("Zip        : " & Natural'Image (P.Addr.Zip));
      Ada.Text_IO.Put_Line ("Country    : " & To_String (P.Addr.Country));
      Ada.Text_IO.Put_Line ("Info       : " & To_String (P.Addr.Info.Name)
                            & "=" & To_String (P.Addr.Info.Value));
   end Print;

   procedure Print (P : in Mapping.Person_Vector.Cursor) is
   begin
      Print (Mapping.Person_Vector.Element (P));
   end Print;

begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: json file...");
      return;
   end if;

   Person_Vector_Mapping.Set_Mapping (Mapping.Get_Person_Mapper);
   Mapper.Add_Mapping ("/list", Person_Vector_Mapping'Unchecked_Access);
   Mapper.Add_Mapping ("/person", Mapping.Get_Person_Mapper.all'Access);
   for I in 1 .. Count loop
      declare
         S    : constant String := Ada.Command_Line.Argument (I);

         List : aliased Mapping.Person_Vector.Vector;
         P    : aliased Mapping.Person;
      begin
         Person_Vector_Mapper.Set_Context (Mapper, List'Unchecked_Access);
         Mapping.Person_Mapper.Set_Context (Mapper, P'Unchecked_Access);
         Reader.Parse (S, Mapper);

         --  The list now contains our elements.
         List.Iterate (Process => Print'Access);
         if List.Length = 0 then
            Print (P);
         end if;

         declare
            Buffer : aliased Util.Streams.Buffered.Output_Buffer_Stream;
            Print  : aliased Util.Streams.Texts.Print_Stream;
            Output : Util.Serialize.IO.JSON.Output_Stream;
         begin
            Buffer.Initialize (Size => 10000);
            Print.Initialize (Buffer'Unchecked_Access);
            Output.Initialize (Print'Unchecked_Access);
            Mapping.Get_Person_Mapper.Write (Output, P);
            Ada.Text_IO.Put_Line ("Person: "
                                  & Util.Streams.Texts.To_String (Print));
         end;

         declare
            Buffer : aliased Util.Streams.Buffered.Output_Buffer_Stream;
            Print  : aliased Util.Streams.Texts.Print_Stream;
            Output : Util.Serialize.IO.JSON.Output_Stream;
         begin
            Buffer.Initialize (Size => 10000);
            Print.Initialize (Buffer'Unchecked_Access);
            Output.Initialize (Print'Unchecked_Access);
            Output.Write ("{""list"":");
            Person_Vector_Mapping.Write (Output, List);
            Output.Write ("}");
            Output.Flush;

            Ada.Text_IO.Put_Line ("IO:");
            Ada.Text_IO.Put_Line (Util.Streams.Texts.To_String (Buffer));
         end;
      end;
   end loop;
end Json;
