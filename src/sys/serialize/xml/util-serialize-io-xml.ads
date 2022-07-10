-----------------------------------------------------------------------
--  util-serialize-io-xml -- XML Serialization Driver
--  Copyright (C) 2011, 2012, 2016, 2017, 2020, 2021, 2022 Stephane Carrez
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

with Sax.Exceptions;
with Sax.Locators;
with Sax.Readers;
with Sax.Attributes;
with Unicode.CES;
with Input_Sources;

with Ada.Streams;
with Ada.Strings.Unbounded;
with Util.Streams.Buffered;
with Util.Streams.Texts;
package Util.Serialize.IO.XML is

   Parse_Error : exception;

   type Parser is new Serialize.IO.Parser with private;

   --  Parse the stream using the JSON parser.
   overriding
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Input_Buffer_Stream'Class;
                    Sink    : in out Reader'Class);

   --  Set the XHTML reader to ignore or not the white spaces.
   --  When set to True, the ignorable white spaces will not be kept.
   procedure Set_Ignore_White_Spaces (Reader : in out Parser;
                                      Value  : in Boolean);

   --  Set the XHTML reader to ignore empty lines.
   procedure Set_Ignore_Empty_Lines (Reader : in out Parser;
                                     Value  : in Boolean);

   --  Get the current location (file and line) to report an error message.
   overriding
   function Get_Location (Handler : in Parser) return String;

   type Xhtml_Reader is new Sax.Readers.Reader with private;

   --  ------------------------------
   --  XML Output Stream
   --  ------------------------------
   --  The <b>Output_Stream</b> provides methods for creating an XML output stream.
   --  The stream object takes care of the XML escape rules.
   type Output_Stream is limited new Util.Serialize.IO.Output_Stream with private;

   --  Set the target output stream.
   procedure Initialize (Stream : in out Output_Stream;
                         Output : in Util.Streams.Texts.Print_Stream_Access);

   --  Flush the buffer (if any) to the sink.
   overriding
   procedure Flush (Stream : in out Output_Stream);

   --  Close the sink.
   overriding
   procedure Close (Stream : in out Output_Stream);

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Output_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Write a character on the response stream and escape that character as necessary.
   procedure Write_Escape (Stream : in out Output_Stream'Class;
                           Char   : in Wide_Wide_Character);

   --  Write the value as a XML string.  Special characters are escaped using the XML
   --  escape rules.
   procedure Write_String (Stream : in out Output_Stream;
                           Value  : in String);

   --  Write the value as a XML string.  Special characters are escaped using the XML
   --  escape rules.
   procedure Write_Wide_String (Stream : in out Output_Stream;
                                Value  : in Wide_Wide_String);

   --  Write the value as a XML string.  Special characters are escaped using the XML
   --  escape rules.
   procedure Write_String (Stream : in out Output_Stream;
                           Value  : in Util.Beans.Objects.Object);

   --  Start a new XML object.
   overriding
   procedure Start_Entity (Stream : in out Output_Stream;
                           Name   : in String);

   --  Terminates the current XML object.
   overriding
   procedure End_Entity (Stream : in out Output_Stream;
                         Name   : in String);

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

   --  Write a XML name/value attribute.
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

   --  Write a XML name/value entity (see Write_Attribute).
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object);

   --  Write an entity with a null value.
   overriding
   procedure Write_Null_Entity (Stream : in out Output_Stream;
                                Name   : in String);

   --  Starts a XML array.
   overriding
   procedure Start_Array (Stream : in out Output_Stream;
                          Name   : in String);

   --  Terminates a XML array.
   overriding
   procedure End_Array (Stream : in out Output_Stream;
                        Name   : in String);

   --  Set the indentation level when writing XML entities.
   procedure Set_Indentation (Stream : in out Output_Stream;
                              Count  : in Natural);

   --  Return the location where the exception was raised.
   function Get_Location (Except : Sax.Exceptions.Sax_Parse_Exception'Class)
                          return String;

private

   overriding
   procedure Warning (Handler : in out Xhtml_Reader;
                      Except  : in Sax.Exceptions.Sax_Parse_Exception'Class);

   overriding
   procedure Error (Handler : in out Xhtml_Reader;
                    Except  : in Sax.Exceptions.Sax_Parse_Exception'Class);

   overriding
   procedure Fatal_Error (Handler : in out Xhtml_Reader;
                          Except  : in Sax.Exceptions.Sax_Parse_Exception'Class);

   overriding
   procedure Set_Document_Locator (Handler : in out Xhtml_Reader;
                                   Loc     : in out Sax.Locators.Locator);

   overriding
   procedure Start_Document (Handler : in out Xhtml_Reader);

   overriding
   procedure End_Document (Handler : in out Xhtml_Reader);

   overriding
   procedure Start_Prefix_Mapping (Handler : in out Xhtml_Reader;
                                   Prefix  : in Unicode.CES.Byte_Sequence;
                                   URI     : in Unicode.CES.Byte_Sequence);

   overriding
   procedure End_Prefix_Mapping (Handler : in out Xhtml_Reader;
                                 Prefix  : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Start_Element (Handler       : in out Xhtml_Reader;
                            Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                            Local_Name    : in Unicode.CES.Byte_Sequence := "";
                            Qname         : in Unicode.CES.Byte_Sequence := "";
                            Atts          : in Sax.Attributes.Attributes'Class);

   overriding
   procedure End_Element (Handler       : in out Xhtml_Reader;
                          Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                          Local_Name    : in Unicode.CES.Byte_Sequence := "";
                          Qname         : in Unicode.CES.Byte_Sequence := "");

   overriding
   procedure Characters (Handler : in out Xhtml_Reader;
                         Ch      : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Ignorable_Whitespace (Handler : in out Xhtml_Reader;
                                   Ch      : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Processing_Instruction (Handler : in out Xhtml_Reader;
                                     Target  : in Unicode.CES.Byte_Sequence;
                                     Data    : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Skipped_Entity (Handler : in out Xhtml_Reader;
                             Name    : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Start_Cdata (Handler : in out Xhtml_Reader);

   overriding
   procedure End_Cdata (Handler : in out Xhtml_Reader);

   overriding
   function Resolve_Entity (Handler   : Xhtml_Reader;
                            Public_Id : Unicode.CES.Byte_Sequence;
                            System_Id : Unicode.CES.Byte_Sequence)
                            return Input_Sources.Input_Source_Access;

   overriding
   procedure Start_DTD (Handler   : in out Xhtml_Reader;
                        Name      : Unicode.CES.Byte_Sequence;
                        Public_Id : Unicode.CES.Byte_Sequence := "";
                        System_Id : Unicode.CES.Byte_Sequence := "");

   procedure Collect_Text (Handler : in out Xhtml_Reader;
                           Content : Unicode.CES.Byte_Sequence);

   type Xhtml_Reader is new Sax.Readers.Reader with record
      Stack_Pos  : Natural := 0;
      Handler    : access Parser'Class;

      Text : Ada.Strings.Unbounded.Unbounded_String;

      --  Whether white spaces can be ignored.
      Ignore_White_Spaces : Boolean := True;

      --  Whether empty lines should be ignored (when white spaces are kept).
      Ignore_Empty_Lines : Boolean := True;

      Sink               : access Reader'Class;
   end record;

   type Parser is new Util.Serialize.IO.Parser with record
      --  The SAX locator to find the current file and line number.
      Locator             : Sax.Locators.Locator;
      Has_Pending_Char   : Boolean := False;
      Pending_Char       : Character;

      --  Whether white spaces can be ignored.
      Ignore_White_Spaces : Boolean := True;

      --  Whether empty lines should be ignored (when white spaces are kept).
      Ignore_Empty_Lines  : Boolean := True;
   end record;

   type Output_Stream is limited new Util.Serialize.IO.Output_Stream with record
      Close_Start : Boolean := False;
      Is_Closed   : Boolean := False;
      Level       : Natural := 0;
      Indent      : Natural := 0;
      Stream      : Util.Streams.Texts.Print_Stream_Access;
   end record;

end Util.Serialize.IO.XML;
