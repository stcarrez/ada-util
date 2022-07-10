-----------------------------------------------------------------------
--  util-serialize-io-xml -- XML Serialization Driver
--  Copyright (C) 2011, 2012, 2013, 2016, 2017, 2020, 2021, 2022 Stephane Carrez
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
with Ada.Characters.Conversions;
with Unicode;
with Unicode.CES.Utf8;

with Util.Log.Loggers;
with Util.Strings;
with Util.Dates.ISO8601;
with Util.Streams.Texts.TR;
with Util.Streams.Texts.WTR;
with Util.Beans.Objects.Maps;
package body Util.Serialize.IO.XML is

   use Sax.Readers;
   use Sax.Exceptions;
   use Sax.Locators;
   use Sax.Attributes;
   use Unicode;
   use Unicode.CES;
   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Serialize.IO.XML");

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

      Handler.Sink.Start_Object (Local_Name, Handler.Handler.all);
      Attr_Count := Get_Length (Atts);
      for I in 0 .. Attr_Count - 1 loop
         declare
            Name  : constant String := Get_Qname (Atts, I);
            Value : constant String := Get_Value (Atts, I);
         begin
            Handler.Sink.Set_Member (Name      => Name,
                                     Value     => Util.Beans.Objects.To_Object (Value),
                                     Logger    => Handler.Handler.all,
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

      Len : constant Natural := Length (Handler.Text);
   begin
      Handler.Sink.Finish_Object (Local_Name, Handler.Handler.all);
      if Len > 0 then

         --  Add debug message only when it is active (saves the To_String conversion).
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Close object {0} -> {1}", Local_Name, To_String (Handler.Text));
         end if;
         Handler.Sink.Set_Member (Local_Name, Util.Beans.Objects.To_Object (Handler.Text),
                                  Handler.Handler.all);

         --  Clear the string using Delete so that the buffer is kept.
         Ada.Strings.Unbounded.Delete (Source => Handler.Text, From => 1, Through => Len);
      else
         Log.Debug ("Close object {0}", Local_Name);
         Handler.Sink.Set_Member (Local_Name, Util.Beans.Objects.To_Object (Handler.Text),
                                  Handler.Handler.all);
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
   overriding
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
   overriding
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Input_Buffer_Stream'Class;
                    Sink    : in out Reader'Class) is

      Buffer_Size : constant Positive := 256;

      type String_Access is access all String (1 .. Buffer_Size);

      type Stream_Input is new Input_Sources.Input_Source with record
         Index    : Natural;
         Last     : Natural;
         Encoding : Unicode.CES.Encoding_Scheme;
         Buffer   : String_Access;
      end record;

      --  Return the next character in the string.
      overriding
      procedure Next_Char (From : in out Stream_Input;
                           C    : out Unicode.Unicode_Char);

      --  True if From is past the last character in the string.
      overriding
      function Eof (From : in Stream_Input) return Boolean;
      procedure Fill (From : in out Stream_Input'Class);

      procedure Fill (From : in out Stream_Input'Class) is
         Last : Natural := From.Last;
      begin
         --  Move to the buffer start
         if Last > From.Index and then From.Index > From.Buffer'First then
            From.Buffer (From.Buffer'First .. Last - 1 - From.Index + From.Buffer'First) :=
              From.Buffer (From.Index .. Last - 1);
            Last  := Last - From.Index + From.Buffer'First;
            From.Index := From.Buffer'First;
         end if;
         if From.Index > From.Last then
            From.Index := From.Buffer'First;
         end if;
         begin
            while not Stream.Is_Eof loop
               Stream.Read (From.Buffer (Last));
               Last := Last + 1;
               exit when Last > From.Buffer'Last;
            end loop;
         exception
            when others =>
               null;
         end;
         From.Last := Last;
      end Fill;

      --  Return the next character in the string.
      overriding
      procedure Next_Char (From : in out Stream_Input;
                           C    : out Unicode.Unicode_Char) is
      begin
         if From.Index + 6 >= From.Last then
            Fill (From);
         end if;
         From.Encoding.Read (From.Buffer.all, From.Index, C);
      end Next_Char;

      --  True if From is past the last character in the string.
      overriding
      function Eof (From : in Stream_Input) return Boolean is
      begin
         if From.Index < From.Last then
            return False;
         end if;
         return Stream.Is_Eof;
      end Eof;

      Input      : Stream_Input;
      Xml_Parser : Xhtml_Reader;
      Buf        : aliased String (1 .. Buffer_Size);
   begin
      Input.Buffer := Buf'Access;
      Input.Index  := Buf'First + 1;
      Input.Last   := Buf'First;
      Input.Set_Encoding (Unicode.CES.Utf8.Utf8_Encoding);
      Input.Encoding := Unicode.CES.Utf8.Utf8_Encoding;
      Xml_Parser.Handler := Handler'Unchecked_Access;
      Xml_Parser.Ignore_White_Spaces := Handler.Ignore_White_Spaces;
      Xml_Parser.Ignore_Empty_Lines  := Handler.Ignore_Empty_Lines;
      Xml_Parser.Sink := Sink'Unchecked_Access;
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
   procedure Close_Current (Stream : in out Output_Stream'Class;
                            Indent : in Boolean);

   --  ------------------------------
   --  Close the current XML entity if an entity was started
   --  ------------------------------
   procedure Close_Current (Stream : in out Output_Stream'Class;
                            Indent : in Boolean) is
   begin
      if Stream.Close_Start then
         Stream.Write ('>');
         Stream.Close_Start := False;
      end if;
      if Indent then
         if Stream.Indent /= 0 then
            Stream.Write (ASCII.LF);
         end if;
         for I in 1 .. Stream.Level loop
            Stream.Write (' ');
         end loop;
      end if;
   end Close_Current;

   --  -----------------------
   --  Set the target output stream.
   --  -----------------------
   procedure Initialize (Stream : in out Output_Stream;
                         Output : in Util.Streams.Texts.Print_Stream_Access) is
   begin
      Stream.Stream := Output;
   end Initialize;

   --  -----------------------
   --  Flush the buffer (if any) to the sink.
   --  -----------------------
   overriding
   procedure Flush (Stream : in out Output_Stream) is
   begin
      Stream.Stream.Flush;
   end Flush;

   --  -----------------------
   --  Close the sink.
   --  -----------------------
   overriding
   procedure Close (Stream : in out Output_Stream) is
   begin
      Stream.Stream.Close;
   end Close;

   --  -----------------------
   --  Write the buffer array to the output stream.
   --  -----------------------
   overriding
   procedure Write (Stream : in out Output_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      Stream.Stream.Write (Buffer);
   end Write;

   --  ------------------------------
   --  Write a character on the response stream and escape that character as necessary.
   --  ------------------------------
   procedure Write_Escape (Stream : in out Output_Stream'Class;
                           Char   : in Wide_Wide_Character) is
      type Unicode_Char is mod 2**32;

      Code : constant Unicode_Char := Wide_Wide_Character'Pos (Char);
   begin
      --  If "?" or over, no escaping is needed (this covers
      --  most of the Latin alphabet)
      if Code >= 16#80# then
         Stream.Write_Wide (Char);
      elsif Code > 16#3F# or else Code <= 16#20# then
         Stream.Write (Character'Val (Code));
      elsif Char = '<' then
         Stream.Write ("&lt;");
      elsif Char = '>' then
         Stream.Write ("&gt;");
      elsif Char = '&' then
         Stream.Write ("&amp;");
      else
         Stream.Write (Character'Val (Code));
      end if;
   end Write_Escape;

   --  ------------------------------
   --  Write the value as a XML string.  Special characters are escaped using the XML
   --  escape rules.
   --  ------------------------------
   procedure Write_String (Stream : in out Output_Stream;
                           Value  : in String) is
   begin
      Close_Current (Stream, False);
      for I in Value'Range loop
         Stream.Write_Escape (Ada.Characters.Conversions.To_Wide_Wide_Character (Value (I)));
      end loop;
   end Write_String;

   --  ------------------------------
   --  Write the value as a XML string.  Special characters are escaped using the XML
   --  escape rules.
   --  ------------------------------
   procedure Write_Wide_String (Stream : in out Output_Stream;
                                Value  : in Wide_Wide_String) is
   begin
      Close_Current (Stream, False);
      for I in Value'Range loop
         Stream.Write_Escape (Value (I));
      end loop;
   end Write_Wide_String;

   --  ------------------------------
   --  Write the value as a XML string.  Special characters are escaped using the XML
   --  escape rules.
   --  ------------------------------
   procedure Write_String (Stream : in out Output_Stream;
                           Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
   begin
      Close_Current (Stream, False);
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
            Stream.Stream.Write (Util.Beans.Objects.To_Long_Long_Integer (Value));

         when others =>
            Stream.Write_String (Util.Beans.Objects.To_String (Value));

      end case;
   end Write_String;

   --  ------------------------------
   --  Start a new XML object.
   --  ------------------------------
   overriding
   procedure Start_Entity (Stream : in out Output_Stream;
                           Name   : in String) is
   begin
      Close_Current (Stream, True);
      Stream.Close_Start := True;
      Stream.Is_Closed := False;
      Stream.Write ('<');
      Stream.Write (Name);
      Stream.Level := Stream.Level + Stream.Indent;
   end Start_Entity;

   --  ------------------------------
   --  Terminates the current XML object.
   --  ------------------------------
   overriding
   procedure End_Entity (Stream : in out Output_Stream;
                         Name   : in String) is
   begin
      Stream.Level := Stream.Level - Stream.Indent;
      Close_Current (Stream, Stream.Is_Closed);
      Stream.Write ("</");
      Stream.Write (Name);
      Stream.Write ('>');
      Stream.Is_Closed := True;
   end End_Entity;

   --  ------------------------------
   --  Write the attribute name/value pair.
   --  ------------------------------
   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in String) is
   begin
      Stream.Write (' ');
      Stream.Write (Name);
      Stream.Write ("=""");
      Util.Streams.Texts.TR.Escape_Xml (Content => Value, Into => Stream.Stream.all);
      Stream.Write ('"');
   end Write_Attribute;

   overriding
   procedure Write_Wide_Attribute (Stream : in out Output_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String) is
   begin
      Stream.Write (' ');
      Stream.Write (Name);
      Stream.Write ("=""");
      Util.Streams.Texts.WTR.Escape_Xml (Content => Value, Into => Stream.Stream.all);
      Stream.Write ('"');
   end Write_Wide_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Integer) is
   begin
      Stream.Write (' ');
      Stream.Write (Name);
      Stream.Write ("=""");
      Stream.Stream.Write (Value);
      Stream.Write ('"');
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Boolean) is
   begin
      Stream.Write (' ');
      Stream.Write (Name);
      if Value then
         Stream.Write ("=""true""");
      else
         Stream.Write ("=""false""");
      end if;
   end Write_Attribute;

   --  ------------------------------
   --  Write a XML name/value attribute.
   --  ------------------------------
   overriding
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
            Stream.Stream.Write (Util.Beans.Objects.To_Long_Long_Integer (Value));

         when others =>
            Stream.Write (Util.Beans.Objects.To_String (Value));

      end case;
      Stream.Write ('"');
   end Write_Attribute;

   --  ------------------------------
   --  Write the attribute with a null value.
   --  ------------------------------
   overriding
   procedure Write_Null_Attribute (Stream : in out Output_Stream;
                                   Name   : in String) is
   begin
      null;
   end Write_Null_Attribute;

   --  ------------------------------
   --  Write the entity value.
   --  ------------------------------
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in String) is
   begin
      Close_Current (Stream, True);
      Stream.Write ('<');
      Stream.Write (Name);
      Stream.Close_Start := True;
      Stream.Write_String (Value);
      Stream.Write ("</");
      Stream.Write (Name);
      Stream.Write ('>');
      Stream.Is_Closed := True;
   end Write_Entity;

   overriding
   procedure Write_Wide_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String) is
   begin
      Close_Current (Stream, True);
      Stream.Write ('<');
      Stream.Write (Name);
      Stream.Close_Start := True;
      Stream.Write_Wide_String (Value);
      Stream.Write ("</");
      Stream.Write (Name);
      Stream.Write ('>');
      Stream.Is_Closed := True;
   end Write_Wide_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Boolean) is
   begin
      Close_Current (Stream, True);
      Stream.Write ('<');
      Stream.Write (Name);
      if Value then
         Stream.Write (">true</");
      else
         Stream.Write (">false</");
      end if;
      Stream.Write (Name);
      Stream.Write ('>');
      Stream.Is_Closed := True;
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Integer) is
   begin
      Close_Current (Stream, True);
      Stream.Write ('<');
      Stream.Write (Name);
      Stream.Write ('>');
      Stream.Stream.Write (Value);
      Stream.Write ("</");
      Stream.Write (Name);
      Stream.Write ('>');
      Stream.Is_Closed := True;
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
      Close_Current (Stream, True);
      Stream.Write ('<');
      Stream.Write (Name);
      Stream.Write ('>');
      Stream.Stream.Write (Value);
      Stream.Write ("</");
      Stream.Write (Name);
      Stream.Write ('>');
      Stream.Is_Closed := True;
   end Write_Long_Entity;

   overriding
   procedure Write_Enum_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in String) is
   begin
      Stream.Write_Entity (Name, Value);
   end Write_Enum_Entity;

   --  ------------------------------
   --  Write a XML name/value entity (see Write_Attribute).
   --  ------------------------------
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
   begin
      case Util.Beans.Objects.Get_Type (Value) is
         when TYPE_NULL =>
            Close_Current (Stream, True);
            Stream.Write ('<');
            Stream.Write (Name);
            Stream.Close_Start := True;
            Stream.Write ("null");
            Stream.Write ("</");
            Stream.Write (Name);
            Stream.Write ('>');
            Stream.Is_Closed := True;

         when TYPE_BOOLEAN =>
            Close_Current (Stream, True);
            Stream.Write ('<');
            Stream.Write (Name);
            Stream.Close_Start := True;
            if Util.Beans.Objects.To_Boolean (Value) then
               Stream.Write ("true");
            else
               Stream.Write ("false");
            end if;
            Stream.Write ("</");
            Stream.Write (Name);
            Stream.Write ('>');
            Stream.Is_Closed := True;

         when TYPE_INTEGER =>
            Close_Current (Stream, True);
            Stream.Write ('<');
            Stream.Write (Name);
            Stream.Close_Start := True;
            Stream.Stream.Write (Util.Beans.Objects.To_Long_Long_Integer (Value));
            Stream.Write ("</");
            Stream.Write (Name);
            Stream.Write ('>');
            Stream.Is_Closed := True;

         when TYPE_BEAN | TYPE_ARRAY =>
            if Is_Array (Value) then
               declare
                  Count : constant Natural := Util.Beans.Objects.Get_Count (Value);
               begin
                  Close_Current (Stream, False);
                  for I in 1 .. Count loop
                     Stream.Write_Entity (Name, Util.Beans.Objects.Get_Value (Value, I));
                  end loop;
               end;
            else
               declare
                  procedure Process (Name : in String; Item : in Object);

                  procedure Process (Name : in String; Item : in Object) is
                  begin
                     Stream.Write_Entity (Name, Item);
                  end Process;
               begin
                  Close_Current (Stream, True);
                  Stream.Write ('<');
                  Stream.Write (Name);
                  Stream.Close_Start := True;
                  Util.Beans.Objects.Maps.Iterate (Value, Process'Access);
                  Stream.Write ("</");
                  Stream.Write (Name);
                  Stream.Write ('>');
                  Stream.Is_Closed := True;
               end;
            end if;

         when others =>
            Close_Current (Stream, True);
            Stream.Write ('<');
            Stream.Write (Name);
            Stream.Close_Start := True;
            Stream.Write_String (Util.Beans.Objects.To_String (Value));
            Stream.Write ("</");
            Stream.Write (Name);
            Stream.Write ('>');
            Stream.Is_Closed := True;

      end case;
   end Write_Entity;

   --  ------------------------------
   --  Write an entity with a null value.
   --  ------------------------------
   overriding
   procedure Write_Null_Entity (Stream : in out Output_Stream;
                                Name   : in String) is
   begin
      null;
   end Write_Null_Entity;

   --  ------------------------------
   --  Starts a XML array.
   --  ------------------------------
   overriding
   procedure Start_Array (Stream : in out Output_Stream;
                          Name   : in String) is
      pragma Unreferenced (Stream, Name);
   begin
      null;
   end Start_Array;

   --  ------------------------------
   --  Terminates a XML array.
   --  ------------------------------
   overriding
   procedure End_Array (Stream : in out Output_Stream;
                        Name   : in String) is
   begin
      null;
   end End_Array;

   --  ------------------------------
   --  Set the indentation level when writing XML entities.
   --  ------------------------------
   procedure Set_Indentation (Stream : in out Output_Stream;
                              Count  : in Natural) is
   begin
      Stream.Indent := Count;
   end Set_Indentation;

end Util.Serialize.IO.XML;
