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
with Mapping;
procedure Json is

   use Ada.Strings.Unbounded;
   use type Mapping.Person_Access;
   use type Ada.Containers.Count_Type;

   Reader : Util.Serialize.IO.JSON.Parser;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;

   procedure Print (P : in Mapping.Person) is
   begin
      Ada.Text_IO.Put_Line ("first_name : " & To_String (P.First_Name));
      Ada.Text_IO.Put_Line ("last_name  : " & To_String (P.Last_Name));
      Ada.Text_IO.Put_Line ("Age        : " & Natural'Image (P.Age));
      Ada.Text_IO.Put_Line ("Street     : " & To_String (P.Addr.Street));
      Ada.Text_IO.Put_Line ("City       : " & To_String (P.Addr.City));
      Ada.Text_IO.Put_Line ("Zip        : " & Natural'Image (P.Addr.Zip));
      Ada.Text_IO.Put_Line ("Country    : " & To_String (P.Addr.Country));
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

   Reader.Add_Mapping ("list", Mapping.Get_Person_Vector_Mapper.all'Access);
   Reader.Add_Mapping ("person", Mapping.Get_Person_Mapper.all'Access);
   for I in 1 .. Count loop
      declare
         S    : constant String := Ada.Command_Line.Argument (I);

         List : aliased Mapping.Person_Vector.Vector;
         P    : aliased Mapping.Person;
      begin
         Mapping.Person_Vector_Mapper.Set_Context (Reader, List'Unchecked_Access);
         Mapping.Person_Mapper.Set_Context (Reader, P'Unchecked_Access);
         Reader.Parse (S);

         --  The list now contains our elements.
         List.Iterate (Process => Print'Access);
         if List.Length = 0 then
            Print (P);
         end if;
      end;
   end loop;
end Json;
