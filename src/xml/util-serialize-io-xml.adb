-----------------------------------------------------------------------
--  util-serialize-io-xml -- XML Serialization Driver
--  Copyright (C) 2011 Stephane Carrez
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

with Unicode;
with Unicode.CES.Utf8;

with Util.Log.Loggers;
with Util.Strings;
package body Util.Serialize.IO.XML is

   use Util.Log;
   use Sax.Readers;
   use Sax.Exceptions;
   use Sax.Locators;
   use Sax.Attributes;
   use Unicode;
   use Unicode.CES;
   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Util.Serialize.IO.XML");

   --  Return the location where the exception was raised.
   function Get_Location (Except : Sax.Exceptions.Sax_Parse_Exception'Class)
                          return String is separate;

   --  ------------------------------
   --  Warning
   --  ------------------------------
   overriding
   procedure Warning (Handler : in out Xhtml_Reader;
                      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
      pragma Warnings (Off, Handler);
   begin
      Log.Warn ("{0}", Get_Message (Except));
   end Warning;

   --  ------------------------------
   --  Error
   --  ------------------------------
   overriding
   procedure Error (Handler : in out Xhtml_Reader;
                    Except  : in Sax.Exceptions.Sax_Parse_Exception'Class) is
      Msg : constant String := Get_Message (Except);
      Pos : constant Natural := Util.Strings.Index (Msg, ' ');
   begin
      --  The SAX error message contains the line+file name.  Remove it because this part
      --  will be added by the <b>Error</b> procedure.
      if Pos > Msg'First and then Msg (Pos - 1) = ':' then
         Handler.Handler.Error (Msg (Pos + 1 .. Msg'Last));
      else
         Handler.Handler.Error (Msg);
      end if;
   end Error;

   --  ------------------------------
   --  Fatal_Error
   --  ------------------------------
   overriding
   procedure Fatal_Error (Handler : in out Xhtml_Reader;
                          Except  : in Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Handler.Error (Except);
   end Fatal_Error;

   --  ------------------------------
   --  Set_Document_Locator
   --  ------------------------------
   overriding
   procedure Set_Document_Locator (Handler : in out Xhtml_Reader;
                                   Loc     : in out Sax.Locators.Locator) is
   begin
      Handler.Handler.Locator := Loc;
   end Set_Document_Locator;

   --  ------------------------------
   --  Start_Document
   --  ------------------------------
   overriding
   procedure Start_Document (Handler : in out Xhtml_Reader) is
   begin
      null;
   end Start_Document;

   --  ------------------------------
   --  End_Document
   --  ------------------------------
   overriding
   procedure End_Document (Handler : in out Xhtml_Reader) is
   begin
      null;
   end End_Document;

   --  ------------------------------
   --  Start_Prefix_Mapping
   --  ------------------------------
   overriding
   procedure Start_Prefix_Mapping (Handler : in out Xhtml_Reader;
                                   Prefix  : in Unicode.CES.Byte_Sequence;
                                   URI     : in Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Start_Prefix_Mapping;

   --  ------------------------------
   --  End_Prefix_Mapping
   --  ------------------------------
   overriding
   procedure End_Prefix_Mapping (Handler : in out Xhtml_Reader;
                                 Prefix  : in Unicode.CES.Byte_Sequence) is
   begin
      null;
   end End_Prefix_Mapping;

   --  ------------------------------
   --  Start_Element
   --  ------------------------------
   overriding
   procedure Start_Element (Handler       : in out Xhtml_Reader;
                            Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                            Local_Name    : in Unicode.CES.Byte_Sequence := "";
                            Qname         : in Unicode.CES.Byte_Sequence := "";
                            Atts          : in Sax.Attributes.Attributes'Class) is
      pragma Unreferenced (Namespace_URI, Qname);

      Attr_Count : Natural;
   begin
      Log.Debug ("Start object {0}", Local_Name);

      Handler.Handler.Start_Object (Local_Name);
      Attr_Count := Get_Length (Atts);
      for I in 0 .. Attr_Count - 1 loop
         declare
            Name  : constant String := Get_Qname (Atts, I);
            Value : constant String := Get_Value (Atts, I);
         begin
            Handler.Handler.Set_Member (Name      => Name,
                                        Value     => Util.Beans.Objects.To_Object (Value),
                                        Attribute => True);
         end;
      end loop;
   end Start_Element;

   --  ------------------------------
   --  End_Element
   --  ------------------------------
   overriding
   procedure End_Element (Handler       : in out Xhtml_Reader;
                          Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                          Local_Name    : in Unicode.CES.Byte_Sequence := "";
                          Qname         : in Unicode.CES.Byte_Sequence := "") is
      pragma Unreferenced (Namespace_URI, Qname);
   begin
      Handler.Handler.Finish_Object (Local_Name);
      if Length (Handler.Text) > 0 then
         Log.Debug ("Close object {0} -> {1}", Local_Name, To_String (Handler.Text));
         Handler.Handler.Set_Member (Local_Name, Util.Beans.Objects.To_Object (Handler.Text));
         Set_Unbounded_String (Handler.Text, "");
      else
         Log.Debug ("Close object {0}", Local_Name);
         Handler.Handler.Set_Member (Local_Name, Util.Beans.Objects.To_Object (Handler.Text));
      end if;
   end End_Element;

   procedure Collect_Text (Handler : in out Xhtml_Reader;
                           Content : Unicode.CES.Byte_Sequence) is
   begin
      Append (Handler.Text, Content);
   end Collect_Text;

   --  ------------------------------
   --  Characters
   --  ------------------------------
   overriding
   procedure Characters (Handler : in out Xhtml_Reader;
                         Ch      : in Unicode.CES.Byte_Sequence) is
   begin
      Collect_Text (Handler, Ch);
   end Characters;

   --  ------------------------------
   --  Ignorable_Whitespace
   --  ------------------------------
   overriding
   procedure Ignorable_Whitespace (Handler : in out Xhtml_Reader;
                                   Ch      : in Unicode.CES.Byte_Sequence) is
   begin
      if not Handler.Ignore_White_Spaces then
         Collect_Text (Handler, Ch);
      end if;
   end Ignorable_Whitespace;

   --  ------------------------------
   --  Processing_Instruction
   --  ------------------------------
   overriding
   procedure Processing_Instruction (Handler : in out Xhtml_Reader;
                                     Target  : in Unicode.CES.Byte_Sequence;
                                     Data    : in Unicode.CES.Byte_Sequence) is
      pragma Unreferenced (Handler);
   begin
      Log.Error ("Processing instruction: {0}: {1}", Target, Data);
   end Processing_Instruction;

   --  ------------------------------
   --  Skipped_Entity
   --  ------------------------------
   overriding
   procedure Skipped_Entity (Handler : in out Xhtml_Reader;
                             Name    : in Unicode.CES.Byte_Sequence) is
      pragma Unmodified (Handler);
   begin
      null;
   end Skipped_Entity;

   --  ------------------------------
   --  Start_Cdata
   --  ------------------------------
   overriding
   procedure Start_Cdata (Handler : in out Xhtml_Reader) is
      pragma Unmodified (Handler);
      pragma Unreferenced (Handler);
   begin
      Log.Info ("Start CDATA");
   end Start_Cdata;

   --  ------------------------------
   --  End_Cdata
   --  ------------------------------
   overriding
   procedure End_Cdata (Handler : in out Xhtml_Reader) is
      pragma Unmodified (Handler);
      pragma Unreferenced (Handler);
   begin
      Log.Info ("End CDATA");
   end End_Cdata;

   --  ------------------------------
   --  Resolve_Entity
   --  ------------------------------
   overriding
   function Resolve_Entity (Handler   : Xhtml_Reader;
                            Public_Id : Unicode.CES.Byte_Sequence;
                            System_Id : Unicode.CES.Byte_Sequence)
                            return Input_Sources.Input_Source_Access is
      pragma Unreferenced (Handler);
   begin
      Log.Error ("Cannot resolve entity {0} - {1}", Public_Id, System_Id);
      return null;
   end Resolve_Entity;

   overriding
   procedure Start_DTD (Handler   : in out Xhtml_Reader;
                        Name      : Unicode.CES.Byte_Sequence;
                        Public_Id : Unicode.CES.Byte_Sequence := "";
                        System_Id : Unicode.CES.Byte_Sequence := "") is
   begin
      null;
   end Start_DTD;

   --  ------------------------------
   --  Set the XHTML reader to ignore or not the white spaces.
   --  When set to True, the ignorable white spaces will not be kept.
   --  ------------------------------
   procedure Set_Ignore_White_Spaces (Reader : in out Parser;
                                      Value  : in Boolean) is
   begin
      Reader.Ignore_White_Spaces := Value;
   end Set_Ignore_White_Spaces;

   --  ------------------------------
   --  Set the XHTML reader to ignore empty lines.
   --  ------------------------------
   procedure Set_Ignore_Empty_Lines (Reader : in out Parser;
                                     Value  : in Boolean) is
   begin
      Reader.Ignore_Empty_Lines := Value;
   end Set_Ignore_Empty_Lines;

   --  ------------------------------
   --  Get the current location (file and line) to report an error message.
   --  ------------------------------
   function Get_Location (Handler : in Parser) return String is
      File : constant String := Util.Serialize.IO.Parser (Handler).Get_Location;
   begin
      if Handler.Locator = Sax.Locators.No_Locator then
         return File;
      else
         return File & Sax.Locators.To_String (Handler.Locator);
      end if;
   end Get_Location;

   --  ------------------------------
   --  Parse an XML stream, and calls the appropriate SAX callbacks for each
   --  event.
   --  This is not re-entrant: you can not call Parse with the same Parser
   --  argument in one of the SAX callbacks. This has undefined behavior.
   --  ------------------------------

   --  Parse the stream using the JSON parser.
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Buffered_Stream'Class) is

      type String_Access is access all String (1 .. 32);

      type Stream_Input is new Input_Sources.Input_Source with record
         Index    : Natural;
         Last     : Natural;
         Encoding : Unicode.CES.Encoding_Scheme;
         Buffer   : String_Access;
      end record;

      --  Return the next character in the string.
      procedure Next_Char (From : in out Stream_Input;
                           C    : out Unicode.Unicode_Char);

      --  True if From is past the last character in the string.
      function Eof (From : in Stream_Input) return Boolean;
      procedure Fill (From : in out Stream_Input'Class);

      procedure Fill (From : in out Stream_Input'Class) is
      begin
         --  Move to the buffer start
         if From.Last > From.Index and From.Index > From.Buffer'First then
            From.Buffer (From.Buffer'First .. From.Last - 1 - From.Index + From.Buffer'First) :=
              From.Buffer (From.Index .. From.Last - 1);
            From.Last  := From.Last - From.Index + From.Buffer'First;
            From.Index := From.Buffer'First;
         end if;
         if From.Index > From.Last then
            From.Index := From.Buffer'First;
         end if;
         begin
            loop
               Stream.Read (From.Buffer (From.Last));
               From.Last := From.Last + 1;
               exit when From.Last > From.Buffer'Last;
            end loop;
         exception
            when others =>
               null;
         end;
      end Fill;

      --  Return the next character in the string.
      procedure Next_Char (From : in out Stream_Input;
                           C    : out Unicode.Unicode_Char) is
      begin
         if From.Index + 6 >= From.Last then
            Fill (From);
         end if;
         From.Encoding.Read (From.Buffer.all, From.Index, C);
      end Next_Char;

      --  True if From is past the last character in the string.
      function Eof (From : in Stream_Input) return Boolean is
      begin
         if From.Index < From.Last then
            return False;
         end if;
         return Stream.Is_Eof;
      end Eof;

      Input      : Stream_Input;
      Xml_Parser : Xhtml_Reader;
      Buf        : aliased String (1 .. 32);
   begin
      Input.Buffer := Buf'Access;
      Input.Index  := 2;
      Input.Last   := 1;
      Input.Set_Encoding (Unicode.CES.Utf8.Utf8_Encoding);
      Input.Encoding := Unicode.CES.Utf8.Utf8_Encoding;
      Xml_Parser.Handler := Handler'Unchecked_Access;
      Xml_Parser.Ignore_White_Spaces := Handler.Ignore_White_Spaces;
      Xml_Parser.Ignore_Empty_Lines  := Handler.Ignore_Empty_Lines;
      Sax.Readers.Reader (Xml_Parser).Parse (Input);
      Handler.Locator := Sax.Locators.No_Locator;

      --  Ignore the Program_Error exception that SAX could raise if we know that the
      --  error was reported.
   exception
      when Program_Error =>
         Handler.Locator := Sax.Locators.No_Locator;
         if not Handler.Has_Error then
            raise;
         end if;

      when others =>
         Handler.Locator := Sax.Locators.No_Locator;
         raise;
   end Parse;

   --  Close the current XML entity if an entity was started
   procedure Close_Current (Stream : in out Output_Stream'Class);

   --  ------------------------------
   --  Close the current XML entity if an entity was started
   --  ------------------------------
   procedure Close_Current (Stream : in out Output_Stream'Class) is
   begin
      if Stream.Close_Start then
         Stream.Write ('>');
         Stream.Close_Start := False;
      end if;
   end Close_Current;

   --  ------------------------------
   --  Write the value as a XML string.  Special characters are escaped using the XML
   --  escape rules.
   --  ------------------------------
   procedure Write_String (Stream : in out Output_Stream;
                           Value  : in String) is
   begin
      Close_Current (Stream);
      Stream.Write (Value);
   end Write_String;

   --  ------------------------------
   --  Start a new XML object.
   --  ------------------------------
   procedure Start_Entity (Stream : in out Output_Stream;
                           Name   : in String) is
   begin
      Close_Current (Stream);
      Stream.Close_Start := True;
      Stream.Write ('<');
      Stream.Write (Name);
   end Start_Entity;

   --  ------------------------------
   --  Terminates the current XML object.
   --  ------------------------------
   procedure End_Entity (Stream : in out Output_Stream;
                         Name   : in String) is
   begin
      Close_Current (Stream);
      Stream.Write ("</");
      Stream.Write (Name);
      Stream.Write ('>');
   end End_Entity;

   --  ------------------------------
   --  Write a XML name/value attribute.
   --  ------------------------------
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
   begin
      Stream.Write (' ');
      Stream.Write (Name);
      Stream.Write ("=""");
      case Util.Beans.Objects.Get_Type (Value) is
         when TYPE_NULL =>
            null;

         when TYPE_BOOLEAN =>
            if Util.Beans.Objects.To_Boolean (Value) then
               Stream.Write ("true");
            else
               Stream.Write ("false");
            end if;

         when TYPE_INTEGER =>
            Stream.Write (Util.Beans.Objects.To_Long_Long_Integer (Value));

         when others =>
            Stream.Write (Util.Beans.Objects.To_String (Value));

      end case;
      Stream.Write ('"');
   end Write_Attribute;

   --  ------------------------------
   --  Write a XML name/value entity (see Write_Attribute).
   --  ------------------------------
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
   begin
      Close_Current (Stream);
      Stream.Write ('<');
      Stream.Write (Name);
      Stream.Write ('>');
      case Util.Beans.Objects.Get_Type (Value) is
         when TYPE_NULL =>
            null;

         when TYPE_BOOLEAN =>
            if Util.Beans.Objects.To_Boolean (Value) then
               Stream.Write ("true");
            else
               Stream.Write ("false");
            end if;

         when TYPE_INTEGER =>
            Stream.Write (Util.Beans.Objects.To_Long_Long_Integer (Value));

         when others =>
            Stream.Write_String (Util.Beans.Objects.To_String (Value));

      end case;
      Stream.Write ("</");
      Stream.Write (Name);
      Stream.Write ('>');
   end Write_Entity;

   --  ------------------------------
   --  Starts a XML array.
   --  ------------------------------
   procedure Start_Array (Stream : in out Output_Stream;
                          Length : in Ada.Containers.Count_Type) is
   begin
      null;
   end Start_Array;

   --  ------------------------------
   --  Terminates a XML array.
   --  ------------------------------
   procedure End_Array (Stream : in out Output_Stream) is
   begin
      null;
   end End_Array;

end Util.Serialize.IO.XML;
