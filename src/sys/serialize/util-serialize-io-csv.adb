-----------------------------------------------------------------------
--  util-serialize-io-csv -- CSV Serialization Driver
--  Copyright (C) 2011, 2015, 2016, 2017, 2021, 2022 Stephane Carrez
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
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;
with Ada.Containers;

with Util.Strings;
with Util.Dates.ISO8601;

package body Util.Serialize.IO.CSV is

   --  ------------------------------
   --  Set the field separator.  The default field separator is the comma (',').
   --  ------------------------------
   procedure Set_Field_Separator (Stream    : in out Output_Stream;
                                  Separator : in Character) is
   begin
      Stream.Separator := Separator;
   end Set_Field_Separator;

   --  ------------------------------
   --  Enable or disable the double quotes by default for strings.
   --  ------------------------------
   procedure Set_Quotes (Stream : in out Output_Stream;
                         Enable : in Boolean) is
   begin
      Stream.Quote := Enable;
   end Set_Quotes;

   --  ------------------------------
   --  Write the value as a CSV cell.  Special characters are escaped using the CSV
   --  escape rules.
   --  ------------------------------
   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in String) is
   begin
      if Stream.Column > 1 then
         Stream.Write (Stream.Separator);
      end if;
      Stream.Column := Stream.Column + 1;
      if Stream.Quote then
         Stream.Write ('"');
      end if;
      for I in Value'Range loop
         if Value (I) = '"' then
            Stream.Write ("""""");
         else
            Stream.Write (Value (I));
         end if;
      end loop;
      if Stream.Quote then
         Stream.Write ('"');
      end if;
   end Write_Cell;

   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in Integer) is
   begin
      if Stream.Column > 1 then
         Stream.Write (Stream.Separator);
      end if;
      Stream.Column := Stream.Column + 1;
      Stream.Write (Util.Strings.Image (Value));
   end Write_Cell;

   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in Boolean) is
   begin
      if Stream.Column > 1 then
         Stream.Write (Stream.Separator);
      end if;
      Stream.Column := Stream.Column + 1;
      if Value then
         Stream.Write ("true");
      else
         Stream.Write ("false");
      end if;
   end Write_Cell;

   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
   begin
      case Util.Beans.Objects.Get_Type (Value) is
         when TYPE_NULL =>
            if Stream.Column > 1 then
               Stream.Write (Stream.Separator);
            end if;
            Stream.Column := Stream.Column + 1;
            if Stream.Quote then
               Stream.Write ("""null""");
            else
               Stream.Write ("null");
            end if;

         when TYPE_BOOLEAN =>
            if Stream.Column > 1 then
               Stream.Write (Stream.Separator);
            end if;
            Stream.Column := Stream.Column + 1;
            if Util.Beans.Objects.To_Boolean (Value) then
               Stream.Write ("true");
            else
               Stream.Write ("false");
            end if;

         when TYPE_INTEGER =>
            if Stream.Column > 1 then
               Stream.Write (Stream.Separator);
            end if;
            Stream.Column := Stream.Column + 1;
            --  Stream.Write ('"');
            Stream.Write (Util.Beans.Objects.To_Long_Long_Integer (Value));
            --  Stream.Write ('"');

         when others =>
            Stream.Write_Cell (Util.Beans.Objects.To_String (Value));

      end case;
   end Write_Cell;

   --  ------------------------------
   --  Start a new row.
   --  ------------------------------
   procedure New_Row (Stream : in out Output_Stream) is
   begin
      while Stream.Column < Stream.Max_Columns loop
         Stream.Write (Stream.Separator);
         Stream.Column := Stream.Column + 1;
      end loop;
      Stream.Write (ASCII.CR);
      Stream.Write (ASCII.LF);
      Stream.Column := 1;
      Stream.Row := Stream.Row + 1;
   end New_Row;

   --  -----------------------
   --  Write the attribute name/value pair.
   --  -----------------------
   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in String) is
      pragma Unreferenced (Name);
   begin
      Stream.Write_Cell (Value);
   end Write_Attribute;

   overriding
   procedure Write_Wide_Attribute (Stream : in out Output_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String) is
   begin
      null;
   end Write_Wide_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Integer) is
      pragma Unreferenced (Name);
   begin
      Stream.Write_Cell (Value);
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Boolean) is
      pragma Unreferenced (Name);
   begin
      Stream.Write_Cell (Value);
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is
      pragma Unreferenced (Name);
   begin
      Stream.Write_Cell (Value);
   end Write_Attribute;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
      pragma Unreferenced (Name);
   begin
      Stream.Write_Cell (Value);
   end Write_Entity;

   --  -----------------------
   --  Write the entity value.
   --  -----------------------
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in String) is
      pragma Unreferenced (Name);
   begin
      Stream.Write_Cell (Value);
   end Write_Entity;

   overriding
   procedure Write_Wide_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String) is
   begin
      null;
   end Write_Wide_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Boolean) is
      pragma Unreferenced (Name);
   begin
      Stream.Write_Cell (Value);
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Integer) is
      pragma Unreferenced (Name);
   begin
      Stream.Write_Cell (Value);
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Ada.Calendar.Time) is
   begin
      Stream.Write_Entity (Name, Util.Dates.ISO8601.Image (Value, Util.Dates.ISO8601.SUBSECOND));
   end Write_Entity;

   overriding
   procedure Write_Long_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Integer) is
   begin
      null;
   end Write_Long_Entity;

   overriding
   procedure Write_Enum_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in String) is
   begin
      Stream.Write_Entity (Name, Value);
   end Write_Enum_Entity;

   --  ------------------------------
   --  Write the attribute with a null value.
   --  ------------------------------
   overriding
   procedure Write_Null_Attribute (Stream : in out Output_Stream;
                                   Name   : in String) is
   begin
      Stream.Write_Entity (Name, "");
   end Write_Null_Attribute;

   --  ------------------------------
   --  Write an entity with a null value.
   --  ------------------------------
   overriding
   procedure Write_Null_Entity (Stream : in out Output_Stream;
                                Name   : in String) is
   begin
      Stream.Write_Null_Attribute (Name);
   end Write_Null_Entity;

   --  ------------------------------
   --  Get the header name for the given column.
   --  If there was no header line, build a default header for the column.
   --  ------------------------------
   function Get_Header_Name (Handler : in Parser;
                             Column  : in Column_Type) return String is
      use type Ada.Containers.Count_Type;

      Default_Header : constant String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      Result : String (1 .. 10);
      N, R   : Natural;
      Pos    : Positive := Result'Last;
   begin
      if Handler.Headers.Length >= Ada.Containers.Count_Type (Column) then
         return Handler.Headers.Element (Positive (Column));
      end if;
      N := Natural (Column - 1);
      loop
         R := N mod 26;
         N := N / 26;
         Result (Pos) := Default_Header (R + 1);
         exit when N = 0;
         Pos := Pos - 1;
      end loop;
      return Result (Pos .. Result'Last);
   end Get_Header_Name;

   --  ------------------------------
   --  Set the cell value at the given row and column.
   --  The default implementation finds the column header name and
   --  invokes <b>Write_Entity</b> with the header name and the value.
   --  ------------------------------
   procedure Set_Cell (Handler : in out Parser;
                       Value   : in String;
                       Row     : in Row_Type;
                       Column  : in Column_Type) is
      use Ada.Containers;
   begin
      if Row = 0 then
         --  Build the headers table.
         declare
            Missing : constant Integer := Integer (Column) - Integer (Handler.Headers.Length);
         begin
            if Missing > 0 then
               Handler.Headers.Set_Length (Handler.Headers.Length + Count_Type (Missing));
            end if;
            Handler.Headers.Replace_Element (Positive (Column), Value);
         end;
      else
         declare
            Name : constant String := Handler.Get_Header_Name (Column);
         begin
            --  Detect a new row.  Close the current object and start a new one.
            if Handler.Row /= Row then
               if Row > 1 then
                  Handler.Sink.Finish_Object ("", Handler);
               else
                  Handler.Sink.Start_Array ("", Handler);
               end if;
               Handler.Sink.Start_Object ("", Handler);
            end if;
            Handler.Row := Row;
            Handler.Sink.Set_Member (Name, Util.Beans.Objects.To_Object (Value), Handler);
         end;
      end if;
   end Set_Cell;

   --  ------------------------------
   --  Set the field separator.  The default field separator is the comma (',').
   --  ------------------------------
   procedure Set_Field_Separator (Handler   : in out Parser;
                                  Separator : in Character) is
   begin
      Handler.Separator := Separator;
   end Set_Field_Separator;

   --  ------------------------------
   --  Get the field separator.
   --  ------------------------------
   function Get_Field_Separator (Handler : in Parser) return Character is
   begin
      return Handler.Separator;
   end Get_Field_Separator;

   --  ------------------------------
   --  Set the comment separator.  When a comment separator is defined, a line which starts
   --  with the comment separator will be ignored.  The row number will not be incremented.
   --  ------------------------------
   procedure Set_Comment_Separator (Handler   : in out Parser;
                                    Separator : in Character) is
   begin
      Handler.Comment := Separator;
   end Set_Comment_Separator;

   --  ------------------------------
   --  Get the comment separator.  Returns ASCII.NUL if comments are not supported.
   --  ------------------------------
   function Get_Comment_Separator (Handler : in Parser) return Character is
   begin
      return Handler.Comment;
   end Get_Comment_Separator;

   --  ------------------------------
   --  Setup the CSV parser and mapper to use the default column header names.
   --  When activated, the first row is assumed to contain the first item to de-serialize.
   --  ------------------------------
   procedure Set_Default_Headers (Handler : in out Parser;
                                  Mode    : in Boolean := True) is
   begin
      Handler.Use_Default_Headers := Mode;
   end Set_Default_Headers;

   --  ------------------------------
   --  Parse the stream using the CSV parser.
   --  Call <b>Set_Cell</b> for each cell that has been parsed indicating the row and
   --  column numbers as well as the cell value.
   --  ------------------------------
   overriding
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Input_Buffer_Stream'Class;
                    Sink    : in out Reader'Class) is
      use Ada.Strings.Unbounded;

      C              : Character;
      Token          : Unbounded_String;
      Column         : Column_Type := 1;
      Row            : Row_Type  := 0;
      In_Quote_Token : Boolean := False;
      In_Escape      : Boolean := False;
      Ignore_Row     : Boolean := False;
   begin
      if Handler.Use_Default_Headers then
         Row := 1;
      end if;
      Handler.Headers.Clear;
      Handler.Sink := Sink'Unchecked_Access;
      loop
         Stream.Read (Char => C);

         if C = Ada.Characters.Latin_1.CR or else C = Ada.Characters.Latin_1.LF then
            if C = Ada.Characters.Latin_1.LF then
               Handler.Line_Number := Handler.Line_Number + 1;
            end if;
            if not Ignore_Row then
               if In_Quote_Token and then not In_Escape then
                  Append (Token, C);

               elsif Column > 1 or else Length (Token) > 0 then
                  Parser'Class (Handler).Set_Cell (To_String (Token), Row, Column);
                  Set_Unbounded_String (Token, "");
                  Row            := Row + 1;
                  Column         := 1;
                  In_Quote_Token := False;
                  In_Escape      := False;
               end if;
            else
               Ignore_Row := False;
            end if;

         elsif C = Handler.Separator and then not Ignore_Row then
            if In_Quote_Token and then not In_Escape then
               Append (Token, C);

            else
               Parser'Class (Handler).Set_Cell (To_String (Token), Row, Column);
               Set_Unbounded_String (Token, "");
               Column         := Column + 1;
               In_Quote_Token := False;
               In_Escape      := False;
            end if;

         elsif C = '"' and then not Ignore_Row then
            if In_Quote_Token then
               In_Escape := True;

            elsif In_Escape then
               Append (Token, C);
               In_Escape := False;

            elsif Ada.Strings.Unbounded.Length (Token) = 0 then
               In_Quote_Token := True;

            else
               Append (Token, C);
            end if;

         elsif C = Handler.Comment and then Handler.Comment /= ASCII.NUL
           and then Column = 1 and then Length (Token) = 0
         then
            Ignore_Row := True;

         elsif not Ignore_Row then
            Append (Token, C);
            In_Escape := False;

         end if;

      end loop;

   exception
      when Ada.IO_Exceptions.Data_Error =>
         Parser'Class (Handler).Set_Cell (To_String (Token), Row, Column);
         Handler.Sink := null;
         return;
   end Parse;

   --  ------------------------------
   --  Get the current location (file and line) to report an error message.
   --  ------------------------------
   overriding
   function Get_Location (Handler : in Parser) return String is
   begin
      return Util.Strings.Image (Handler.Line_Number);
   end Get_Location;

end Util.Serialize.IO.CSV;
