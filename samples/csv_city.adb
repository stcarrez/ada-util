-----------------------------------------------------------------------
--  csv_city -- Read CSV file which contains city mapping
--  Copyright (C) 2011, 2017, 2022 Stephane Carrez
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
with Ada.Containers;
with Ada.Strings.Unbounded;

with Util.Log.Loggers;
with Util.Serialize.IO.CSV;
with Util.Serialize.Mappers;

with City_Mapping;
procedure CSV_City is

   use Ada.Containers;
   use Ada.Strings.Unbounded;
   use Util.Serialize.IO.CSV;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;
begin
   Util.Log.Loggers.Initialize ("samples/log4j.properties");

   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: csv_city file [city...]");
      Ada.Text_IO.Put_Line ("Example: csv_city samples/cities.csv albertville");
      return;
   end if;
   declare
      File   : constant String := Ada.Command_Line.Argument (1);
      List   : aliased City_Mapping.City_Vector.Vector;
      Mapper : aliased Util.Serialize.Mappers.Processing;
      Reader : Util.Serialize.IO.CSV.Parser;
   begin
      Mapper.Add_Mapping ("", City_Mapping.Get_City_Vector_Mapper.all'Access);
      City_Mapping.City_Vector_Mapper.Set_Context (Mapper, List'Unchecked_Access);
      Reader.Parse (File, Mapper);

      if List.Length = 0 then
         Ada.Text_IO.Put_Line ("No city found.");

      elsif List.Length = 1 then
         Ada.Text_IO.Put_Line ("Found only one city.");

      else
         Ada.Text_IO.Put_Line ("Found " & Count_Type'Image (List.Length) & " cities");

      end if;
      for I in 2 .. Count loop
         declare
            Name  : constant String := Ada.Command_Line.Argument (I);
            Found : Boolean := False;

            procedure Print (City : in City_Mapping.City);

            procedure Print (City : in City_Mapping.City) is
            begin
               Found := City.City = Name;
               if Found then
                  Ada.Text_IO.Put_Line ("City        : " & To_String (City.Name));
                  Ada.Text_IO.Put_Line ("Country code: " & To_String (City.Country));
                  Ada.Text_IO.Put_Line ("Region      : " & To_String (City.Region));
                  Ada.Text_IO.Put_Line ("Latitude    : " & Float'Image (City.Latitude));
                  Ada.Text_IO.Put_Line ("Longitude   : " & Float'Image (City.Longitude));
               end if;
            end Print;

         begin
            for J in 1 .. Positive (List.Length) loop
               List.Query_Element (J, Print'Access);
               exit when Found;
            end loop;
            if not Found then
               Ada.Text_IO.Put_Line ("City '" & Name & "' not found");
            end if;
         end;
      end loop;
   end;
end CSV_City;
