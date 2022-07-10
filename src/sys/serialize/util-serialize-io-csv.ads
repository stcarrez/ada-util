-----------------------------------------------------------------------
--  util-serialize-io-csv -- CSV Serialization Driver
--  Copyright (C) 2011, 2016, 2017, 2022 Stephane Carrez
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

with Util.Strings.Vectors;
with Util.Streams.Texts;

--  The <b>Util.Serialize.IO.CSV</b> package allows to read and write CSV files.
--
--  See RFC 4180 - Common Format and MIME Type for Comma-Separated Values (CSV) Files
package Util.Serialize.IO.CSV is

   type Row_Type is new Natural;
   type Column_Type is new Positive;

   --  ------------------------------
   --  CSV Output Stream
   --  ------------------------------
   --  The <b>Output_Stream</b> provides methods for creating a CSV output stream.
   --  The stream object takes care of the CSV escape rules.
   type Output_Stream is
     new Util.Streams.Texts.Print_Stream and Util.Serialize.IO.Output_Stream with private;

   --  Set the field separator.  The default field separator is the comma (',').
   procedure Set_Field_Separator (Stream    : in out Output_Stream;
                                  Separator : in Character);

   --  Enable or disable the double quotes by default for strings.
   procedure Set_Quotes (Stream : in out Output_Stream;
                         Enable : in Boolean);

   --  Write the value as a CSV cell.  Special characters are escaped using the CSV
   --  escape rules.
   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in String);
   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in Integer);
   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in Boolean);
   procedure Write_Cell (Stream : in out Output_Stream;
                         Value  : in Util.Beans.Objects.Object);

   --  Start a new row.
   procedure New_Row (Stream : in out Output_Stream);

   --  Write the attribute name/value pair.
   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in String);

   overriding
   procedure Write_Wide_Attribute (Stream : in out Output_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String);

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Integer);

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Boolean);

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object);

   --  Write the attribute with a null value.
   overriding
   procedure Write_Null_Attribute (Stream : in out Output_Stream;
                                   Name   : in String);

   --  Write the entity value.
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in String);

   overriding
   procedure Write_Wide_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String);

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Boolean);

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Integer);

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Ada.Calendar.Time);

   overriding
   procedure Write_Long_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Integer);

   overriding
   procedure Write_Enum_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in String);

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object);

   --  Write an entity with a null value.
   overriding
   procedure Write_Null_Entity (Stream : in out Output_Stream;
                                Name   : in String);

   --  ------------------------------
   --  CSV Parser
   --  ------------------------------
   --  The <b>Parser</b> type is a CSV parser which allows to map CVS rows directly
   --  in Ada records.
   type Parser is new Serialize.IO.Parser with private;

   --  Get the header name for the given column.
   --  If there was no header line, build a default header for the column.
   function Get_Header_Name (Handler : in Parser;
                             Column  : in Column_Type) return String;

   --  Set the cell value at the given row and column.
   --  The default implementation finds the column header name and
   --  invokes <b>Write_Entity</b> with the header name and the value.
   procedure Set_Cell (Handler : in out Parser;
                       Value   : in String;
                       Row     : in Row_Type;
                       Column  : in Column_Type);

   --  Set the field separator.  The default field separator is the comma (',').
   procedure Set_Field_Separator (Handler   : in out Parser;
                                  Separator : in Character);

   --  Get the field separator.
   function Get_Field_Separator (Handler : in Parser) return Character;

   --  Set the comment separator.  When a comment separator is defined, a line which starts
   --  with the comment separator will be ignored.  The row number will not be incremented.
   procedure Set_Comment_Separator (Handler   : in out Parser;
                                    Separator : in Character);

   --  Get the comment separator.  Returns ASCII.NUL if comments are not supported.
   function Get_Comment_Separator (Handler   : in Parser) return Character;

   --  Setup the CSV parser and mapper to use the default column header names.
   --  When activated, the first row is assumed to contain the first item to de-serialize.
   procedure Set_Default_Headers (Handler : in out Parser;
                                  Mode    : in Boolean := True);

   --  Parse the stream using the CSV parser.
   overriding
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Input_Buffer_Stream'Class;
                    Sink    : in out Reader'Class);

   --  Get the current location (file and line) to report an error message.
   overriding
   function Get_Location (Handler : in Parser) return String;

private

   type Output_Stream is
     new Util.Streams.Texts.Print_Stream and Util.Serialize.IO.Output_Stream with record
      Max_Columns : Column_Type := 1;
      Column      : Column_Type := 1;
      Row         : Row_Type := 0;
      Separator   : Character := ',';
      Quote       : Boolean := True;
   end record;

   type Parser is new Util.Serialize.IO.Parser with record
      Has_Header  : Boolean := True;
      Line_Number : Natural  := 1;
      Row         : Row_Type := 0;
      Headers     : Util.Strings.Vectors.Vector;
      Separator   : Character := ',';
      Comment     : Character := ASCII.NUL;
      Use_Default_Headers : Boolean := False;
      Sink        : access Reader'Class;
   end record;

end Util.Serialize.IO.CSV;
