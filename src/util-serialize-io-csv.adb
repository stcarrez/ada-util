-----------------------------------------------------------------------
--  util-serialize-io-csv -- CSV Serialization Driver
--  Copyright (C) 2011, 2015 Stephane Carrez
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

with Util.Strings;

package body Util.Serialize.IO.CSV is

   --  ------------------------------
   --  Write the value as a CSV cell.  Special characters are escaped using the CSV
   --  escape rules.
   --  ------------------------------
   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in String) is
   begin
      if Stream.Column > 1 then
         Stream.Write (",");
      end if;
      Stream.Column := Stream.Column + 1;
      Stream.Write ('"');
      for I in Value'Range loop
         if Value (I) = '"' then
            Stream.Write ("""""");
         else
            Stream.Write (Value (I));
         end if;
      end loop;
      Stream.Write ('"');
   end Write_Cell;

   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
   begin
      case Util.Beans.Objects.Get_Type (Value) is
         when TYPE_NULL =>
            if Stream.Column > 1 then
               Stream.Write (",");
            end if;
            Stream.Column := Stream.Column + 1;
            Stream.Write ("""null""");

         when TYPE_BOOLEAN =>
            if Stream.Column > 1 then
               Stream.Write (",");
            end if;
            Stream.Column := Stream.Column + 1;
            if Util.Beans.Objects.To_Boolean (Value) then
               Stream.Write ("""true""");
            else
               Stream.Write ("""false""");
            end if;

         when TYPE_INTEGER =>
            if Stream.Column > 1 then
               Stream.Write (",");
            end if;
            Stream.Column := Stream.Column + 1;
            Stream.Write ('"');
            Stream.Write (Util.Beans.Objects.To_Long_Long_Integer (Value));
            Stream.Write ('"');

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
         Stream.Write (",");
         Stream.Column := Stream.Column + 1;
      end loop;
      Stream.Write (ASCII.CR);
      Stream.Write (ASCII.LF);
      Stream.Column := 1;
      Stream.Row := Stream.Row + 1;
   end New_Row;

   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is
   begin
      null;
   end Write_Attribute;

   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
   begin
      null;
   end Write_Entity;

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
                  Parser'Class (Handler).Finish_Object ("");
               end if;
               Parser'Class (Handler).Start_Object ("");
            end if;
            Handler.Row := Row;
            Parser'Class (Handler).Set_Member (Name, Util.Beans.Objects.To_Object (Value));
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
                    Stream  : in out Util.Streams.Buffered.Buffered_Stream'Class) is
      use Ada.Strings.Unbounded;

      C              : Character;
      Token          : Unbounded_String;
      Column         : Column_Type := 1;
      Row            : Row_Type  := 0;
      In_Quote_Token : Boolean := False;
      In_Escape      : Boolean := False;
      Ignore_Row     : Boolean := False;
      Context        : Element_Context_Access;
   begin
      Context_Stack.Push (Handler.Stack);
      Context := Context_Stack.Current (Handler.Stack);
      Context.Active_Nodes (1) := Handler.Mapping_Tree'Unchecked_Access;
      if Handler.Use_Default_Headers then
         Row := 1;
      end if;
      Handler.Headers.Clear;
      loop
         Stream.Read (Char => C);

         if C = Ada.Characters.Latin_1.CR  or C = Ada.Characters.Latin_1.LF then
            if C = Ada.Characters.Latin_1.LF then
               Handler.Line_Number := Handler.Line_Number + 1;
            end if;
            if not Ignore_Row then
               if In_Quote_Token and not In_Escape then
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

         elsif C = Handler.Separator and not Ignore_Row then
            if In_Quote_Token and not In_Escape then
               Append (Token, C);

            else
               Parser'Class (Handler).Set_Cell (To_String (Token), Row, Column);
               Set_Unbounded_String (Token, "");
               Column         := Column + 1;
               In_Quote_Token := False;
               In_Escape      := False;
            end if;

         elsif C = '"' and not Ignore_Row then
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

         elsif C = Handler.Comment and Handler.Comment /= ASCII.NUL
           and Column = 1 and Length (Token) = 0
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
         Context_Stack.Pop (Handler.Stack);
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
