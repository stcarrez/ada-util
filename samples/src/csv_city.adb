-----------------------------------------------------------------------
--  csv_city -- Read CSV file which contains city mapping
--  Copyright (C) 2011, 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
