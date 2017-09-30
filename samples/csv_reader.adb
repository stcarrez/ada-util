-----------------------------------------------------------------------
--  csv_reader -- Read CSV file
--  Copyright (C) 2011, 2017 Stephane Carrez
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

with Util.Serialize.IO.CSV;
with Util.Serialize.Mappers;

--  This example shows how to read a CSV file.  Unlike <b>csv_city</b>, the CSV cells
--  are collected directly by overriding the <b>Set_Cell</b> parser procedure.
procedure CSV_Reader is

   use Ada.Text_IO;
   use Util.Serialize.IO.CSV;

   Prev_Row : Row_Type;

   type CSV_Parser is new Util.Serialize.IO.CSV.Parser with null record;

   overriding
   procedure Set_Cell (Parser : in out CSV_Parser;
                       Value  : in String;
                       Row    : in Util.Serialize.IO.CSV.Row_Type;
                       Column : in Util.Serialize.IO.CSV.Column_Type);

   overriding
   procedure Set_Cell (Parser : in out CSV_Parser;
                       Value  : in String;
                       Row    : in Util.Serialize.IO.CSV.Row_Type;
                       Column : in Util.Serialize.IO.CSV.Column_Type) is
      pragma Unreferenced (Parser, Column);
   begin
      if Prev_Row /= Row then
         Ada.Text_IO.New_Line;
         Prev_Row := Row;
      else
         Put (" ");
      end if;
      Ada.Text_IO.Put (Value);
   end Set_Cell;

   Parser : CSV_Parser;
   Count  : constant Natural := Ada.Command_Line.Argument_Count;

begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: csv_reader file...");
      return;
   end if;
   for I in 1 .. Count loop
      declare
         File : constant String := Ada.Command_Line.Argument (I);
         Mapper : Util.Serialize.Mappers.Processing;
      begin
         Prev_Row := Row_Type'Last;
         Parser.Parse (File, Mapper);
      end;
   end loop;
end CSV_Reader;
