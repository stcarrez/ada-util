-----------------------------------------------------------------------
--  csv_reader -- Read CSV file
--  Copyright (C) 2011, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
