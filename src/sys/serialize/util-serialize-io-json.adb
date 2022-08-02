-----------------------------------------------------------------------
--  util-serialize-io-json -- JSON Serialization Driver
--  Copyright (C) 2010, 2011, 2012, 2016, 2017, 2020, 2021, 2022 Stephane Carrez
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

with Interfaces;

with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.IO_Exceptions;

with Util.Strings;
with Util.Streams;
with Util.Streams.Buffered;
with Util.Streams.Texts.TR;
with Util.Streams.Texts.WTR;
with Util.Dates.ISO8601;
with Util.Beans.Objects.Iterators;
with Util.Beans.Objects.Readers;
package body Util.Serialize.IO.JSON is

   use Ada.Strings.Unbounded;

   package UBO renames Util.Beans.Objects;

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

   --  -----------------------
   --  Write a wide character on the stream doing some conversion if necessary.
   --  The default implementation translates the wide character to a UTF-8 sequence.
   --  -----------------------
   procedure Write_Wide (Stream : in out Output_Stream;
                         Item   : in Wide_Wide_Character) is
   begin
      Stream.Stream.Write_Wide (Item);
   end Write_Wide;

   --  -----------------------
   --  Start a JSON document.  This operation writes the initial JSON marker ('{').
   --  -----------------------
   overriding
   procedure Start_Document (Stream : in out Output_Stream) is
      Current : access Node_Info;
   begin
      Node_Info_Stack.Push (Stream.Stack);
      Current := Node_Info_Stack.Current (Stream.Stack);
      Current.Is_Root := True;
   end Start_Document;

   --  -----------------------
   --  Finish a JSON document by writing the final JSON marker ('}').
   --  -----------------------
   overriding
   procedure End_Document (Stream : in out Output_Stream) is
      Current : constant access Node_Info := Node_Info_Stack.Current (Stream.Stack);
   begin
      if Current /= null and then Current.Has_Fields and then not Current.Is_Array then
         Stream.Write ('}');
      end if;
      Node_Info_Stack.Pop (Stream.Stack);
   end End_Document;

   --  -----------------------
   --  Write the string as a quoted JSON string
   --  -----------------------
   procedure Write_String (Stream : in out Output_Stream;
                           Value  : in String) is
   begin
      Stream.Write ('"');
      for I in Value'Range loop
         declare
            C : constant Character := Value (I);
         begin
            if C = '"' then
               Stream.Write ("\""");

            elsif C = '\'  then
               Stream.Write ("\\");

            elsif Character'Pos (C) >= 16#20# then
               Stream.Write (C);

            else
               case C is
                  when Ada.Characters.Latin_1.BS =>
                     Stream.Write ("\b");

                  when Ada.Characters.Latin_1.VT =>
                     Stream.Write ("\f");

                  when Ada.Characters.Latin_1.LF =>
                     Stream.Write ("\n");

                  when Ada.Characters.Latin_1.CR =>
                     Stream.Write ("\r");

                  when Ada.Characters.Latin_1.HT =>
                     Stream.Write ("\t");

                  when others =>
                     Util.Streams.Texts.TR.To_Hex (Stream.Stream.all, C);

               end case;
            end if;
         end;
      end loop;
      Stream.Write ('"');
   end Write_String;

   --  -----------------------
   --  Write the value as a JSON string.  Special characters are escaped using the JSON
   --  escape rules.
   --  -----------------------
   procedure Write_Wide_String (Stream : in out Output_Stream;
                                Value  : in Wide_Wide_String) is
   begin
      Stream.Write ('"');
      for I in Value'Range loop
         declare
            C : constant Wide_Wide_Character := Value (I);
         begin
            if C = '"' then
               Stream.Write ("\""");

            elsif C = '\'  then
               Stream.Write ("\\");

            elsif Wide_Wide_Character'Pos (C) >= 16#20# then
               Util.Streams.Texts.Write_Char (Stream.Stream.all, C);

            else
               case C is
                  when Ada.Characters.Wide_Wide_Latin_1.BS =>
                     Stream.Write ("\b");

                  when Ada.Characters.Wide_Wide_Latin_1.VT =>
                     Stream.Write ("\f");

                  when Ada.Characters.Wide_Wide_Latin_1.LF =>
                     Stream.Write ("\n");

                  when Ada.Characters.Wide_Wide_Latin_1.CR =>
                     Stream.Write ("\r");

                  when Ada.Characters.Wide_Wide_Latin_1.HT =>
                     Stream.Write ("\t");

                  when others =>
                     Util.Streams.Texts.WTR.To_Hex (Stream.Stream.all, C);

               end case;
            end if;
         end;
      end loop;
      Stream.Write ('"');
   end Write_Wide_String;

   procedure Write_Field_Name (Stream : in out Output_Stream;
                               Name   : in String) is
      Current : constant access Node_Info := Node_Info_Stack.Current (Stream.Stack);
   begin
      if Current /= null then
         if Current.Has_Fields then
            Stream.Write (',');
         elsif Name'Length > 0 or else not Current.Is_Root then
            Current.Has_Fields := True;
         end if;
      end if;

      if (Name'Length > 0 and then (Current = null or else not Current.Is_Array))
        or else (Name'Length = 0 and then Current /= null and then not Current.Is_Array
                 and then not Current.Is_Root)
      then
         Stream.Write_String (Name);
         Stream.Write (':');
      end if;
   end Write_Field_Name;

   --  -----------------------
   --  Start writing an object identified by the given name
   --  -----------------------
   overriding
   procedure Start_Entity (Stream : in out Output_Stream;
                           Name   : in String) is
      Current : access Node_Info := Node_Info_Stack.Current (Stream.Stack);
   begin
      if Current /= null and then Current.Is_Root then
         if Name'Length > 0 then
            Stream.Write ('{');
            Stream.Write_Field_Name (Name);
            Current.Has_Fields := True;
         end if;
      else
         Stream.Write_Field_Name (Name);
      end if;
      Node_Info_Stack.Push (Stream.Stack);
      Current := Node_Info_Stack.Current (Stream.Stack);
      Current.Has_Fields := False;
      Current.Is_Array := False;
      Current.Is_Root := False;
      Stream.Write ('{');
   end Start_Entity;

   --  -----------------------
   --  Finish writing an object identified by the given name
   --  -----------------------
   overriding
   procedure End_Entity (Stream : in out Output_Stream;
                         Name   : in String) is
      pragma Unreferenced (Name);
   begin
      Node_Info_Stack.Pop (Stream.Stack);
      Stream.Write ('}');
   end End_Entity;

   --  -----------------------
   --  Write the attribute name/value pair.
   --  -----------------------
   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in String) is
   begin
      Stream.Write_Field_Name (Name);
      Stream.Write_String (Value);
   end Write_Attribute;

   overriding
   procedure Write_Wide_Attribute (Stream : in out Output_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String) is
   begin
      Stream.Write_Field_Name (Name);
      Stream.Write_Wide_String (Value);
   end Write_Wide_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Integer) is
   begin
      Stream.Write_Field_Name (Name);
      Stream.Write (Integer'Image (Value));
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Boolean) is
   begin
      Stream.Write_Field_Name (Name);
      if Value then
         Stream.Write ("true");
      else
         Stream.Write ("false");
      end if;
   end Write_Attribute;

   --  -----------------------
   --  Write an attribute member from the current object
   --  -----------------------
   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
   begin
      Stream.Write_Field_Name (Name);
      case Util.Beans.Objects.Get_Type (Value) is
         when TYPE_NULL =>
            Stream.Write ("null");

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
   end Write_Attribute;

   --  -----------------------
   --  Write the attribute with a null value.
   --  -----------------------
   overriding
   procedure Write_Null_Attribute (Stream : in out Output_Stream;
                                   Name   : in String) is
   begin
      Stream.Write_Field_Name (Name);
      Stream.Write ("null");
   end Write_Null_Attribute;

   --  -----------------------
   --  Write an object value as an entity
   --  -----------------------
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
   begin
      case Util.Beans.Objects.Get_Type (Value) is
         when TYPE_NULL =>
            Stream.Write_Field_Name (Name);
            Stream.Write ("null");

         when TYPE_BOOLEAN =>
            Stream.Write_Field_Name (Name);
            if Util.Beans.Objects.To_Boolean (Value) then
               Stream.Write ("true");
            else
               Stream.Write ("false");
            end if;

         when TYPE_INTEGER =>
            Stream.Write_Field_Name (Name);
            Stream.Stream.Write (Util.Beans.Objects.To_Long_Long_Integer (Value));

         when TYPE_FLOAT =>
            Stream.Write_Field_Name (Name);
            Stream.Stream.Write
              (Long_Long_Float'Image (Util.Beans.Objects.To_Long_Long_Float (Value)));

         when TYPE_BEAN | TYPE_ARRAY =>
            if Is_Array (Value) then
               Stream.Start_Array (Name);
               declare
                  Count : constant Natural := Util.Beans.Objects.Get_Count (Value);
               begin
                  for I in 1 .. Count loop
                     Stream.Write_Entity ("", Util.Beans.Objects.Get_Value (Value, I));
                  end loop;
               end;
               Stream.End_Array (Name);
            else
               declare
                  Iter : Util.Beans.Objects.Iterators.Iterator
                    := Util.Beans.Objects.Iterators.First (Value);
               begin
                  if not UBO.Iterators.Has_Key (Iter) then
                     Stream.Start_Array (Name);
                     while UBO.Iterators.Has_Element (Iter) loop
                        Stream.Write_Entity ("", UBO.Iterators.Element (Iter));
                        UBO.Iterators.Next (Iter);
                     end loop;
                     Stream.End_Array (Name);
                  else
                     Stream.Start_Entity (Name);
                     while UBO.Iterators.Has_Element (Iter) loop
                        Stream.Write_Entity (UBO.Iterators.Key (Iter),
                                             UBO.Iterators.Element (Iter));
                        UBO.Iterators.Next (Iter);
                     end loop;
                     Stream.End_Entity (Name);
                  end if;
               end;
            end if;

         when others =>
            Stream.Write_Field_Name (Name);
            Stream.Write_String (Util.Beans.Objects.To_String (Value));

      end case;
   end Write_Entity;

   --  -----------------------
   --  Write a JSON name/value pair (see Write_Attribute).
   --  -----------------------
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in String) is
   begin
      Stream.Write_Attribute (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Wide_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String) is
   begin
      Stream.Write_Wide_Attribute (Name, Value);
   end Write_Wide_Entity;

   --  -----------------------
   --  Write a JSON name/value pair (see Write_Attribute).
   --  -----------------------
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Integer) is
   begin
      Stream.Write_Attribute (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Ada.Calendar.Time) is
   begin
      Stream.Write_Entity (Name, Util.Dates.ISO8601.Image (Value, Util.Dates.ISO8601.SUBSECOND));
   end Write_Entity;

   --  -----------------------
   --  Write an entity with a null value.
   --  -----------------------
   overriding
   procedure Write_Null_Entity (Stream : in out Output_Stream;
                                Name   : in String) is
   begin
      Stream.Write_Null_Attribute (Name);
   end Write_Null_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Boolean) is
   begin
      Stream.Write_Attribute (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Long_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Integer) is
   begin
      Stream.Write_Field_Name (Name);
      Stream.Write (Long_Long_Integer'Image (Value));
   end Write_Long_Entity;

   overriding
   procedure Write_Enum_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in String) is
   begin
      Stream.Write_Entity (Name, Value);
   end Write_Enum_Entity;

   --  -----------------------
   --  Start an array that will contain the specified number of elements
   --  Example:  "list": [
   --  -----------------------
   overriding
   procedure Start_Array (Stream : in out Output_Stream;
                          Name   : in String) is
      Current : access Node_Info := Node_Info_Stack.Current (Stream.Stack);
   begin
      if Current /= null then
         if Current.Has_Fields then
            Stream.Write (',');
         elsif not Current.Is_Root then
            Current.Has_Fields := True;
         elsif Name'Length > 0 then
            Stream.Write ('{');
            Current.Has_Fields := True;
         else
            Current.Is_Array := True;
            Current.Has_Fields := True;
         end if;
      end if;
      Node_Info_Stack.Push (Stream.Stack);
      Current := Node_Info_Stack.Current (Stream.Stack);
      Current.Has_Fields := False;
      Current.Is_Array := True;
      Current.Is_Root := False;
      if Name'Length > 0 then
         Stream.Write_String (Name);
         Stream.Write (':');
      end if;
      Stream.Write ('[');
   end Start_Array;

   --  -----------------------
   --  Finishes an array
   --  -----------------------
   overriding
   procedure End_Array (Stream : in out Output_Stream;
                        Name   : in String) is
      pragma Unreferenced (Name);
   begin
      Node_Info_Stack.Pop (Stream.Stack);
      Stream.Write (']');
   end End_Array;

   --  -----------------------
   --  Get the current location (file and line) to report an error message.
   --  -----------------------
   overriding
   function Get_Location (Handler : in Parser) return String is
   begin
      return Util.Strings.Image (Handler.Line_Number);
   end Get_Location;

   overriding
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Input_Buffer_Stream'Class;
                    Sink    : in out Reader'Class) is

      --  Put back a token in the buffer.
      procedure Put_Back (P     : in out Parser'Class;
                          Token : in Token_Type);

      --  Parse the expression buffer to find the next token.
      procedure Peek (P     : in out Parser'Class;
                      Token : out Token_Type);

      function Hexdigit (C : in Character) return Interfaces.Unsigned_32;

      --  Parse a list of members
      --  members ::= pair | pair ',' members
      --  pair    ::= string ':' value
      --  value   ::= string | number | object | array | true | false | null
      procedure Parse_Pairs (P : in out Parser'Class);

      --  Parse a value
      --  value   ::= string | number | object | array | true | false | null
      procedure Parse_Value (P    : in out Parser'Class;
                             Name : in String);

      --  ------------------------------
      --  Parse a list of members
      --  members ::= pair | pair ',' members
      --  pair    ::= string ':' value
      --  value   ::= string | number | object | array | true | false | null
      --  ------------------------------
      procedure Parse_Pairs (P : in out Parser'Class) is
         Current_Name : Unbounded_String;
         Token        : Token_Type;
      begin
         loop
            Peek (P, Token);
            if Token /= T_STRING then
               Put_Back (P, Token);
               return;
            end if;
            Current_Name := P.Token;
            Peek (P, Token);
            if Token /= T_COLON then
               P.Error ("Missing ':'");
            end if;

            Parse_Value (P, To_String (Current_Name));
            Peek (P, Token);
            if Token /= T_COMMA then
               Put_Back (P, Token);
               return;
            end if;
         end loop;
      end Parse_Pairs;

      function Hexdigit (C : in Character) return Interfaces.Unsigned_32 is
         use type Interfaces.Unsigned_32;
      begin
         if C >= '0' and then C <= '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif C >= 'a' and then C <= 'f' then
            return Character'Pos (C) - Character'Pos ('a') + 10;
         elsif C >= 'A' and then C <= 'F' then
            return Character'Pos (C) - Character'Pos ('A') + 10;
         else
            raise Constraint_Error with "Invalid hexdigit: " & C;
         end if;
      end Hexdigit;

      --  ------------------------------
      --  Parse a value
      --  value   ::= string | number | object | array | true | false | null
      --  ------------------------------
      procedure Parse_Value (P    : in out Parser'Class;
                             Name : in String) is
         Token : Token_Type;
         Index : Natural;
      begin
         Peek (P, Token);
         case Token is
            when T_LEFT_BRACE =>
               Sink.Start_Object (Name, P);
               Parse_Pairs (P);
               Peek (P, Token);
               if Token /= T_RIGHT_BRACE then
                  P.Error ("Missing '}'");
               end if;
               Sink.Finish_Object (Name, P);

                  --
            when T_LEFT_BRACKET =>
               Sink.Start_Array (Name, P);
               Peek (P, Token);
               Index := 0;
               if Token /= T_RIGHT_BRACKET then
                  Put_Back (P, Token);
                  loop
                     Parse_Value (P, Util.Strings.Image (Index));
                     Peek (P, Token);
                     exit when Token = T_RIGHT_BRACKET;
                     if Token /= T_COMMA then
                        P.Error ("Missing ']'");
                        exit when Token = T_EOF;
                     end if;
                     Index := Index + 1;
                  end loop;
               end if;
               Sink.Finish_Array (Name, Index, P);

            when T_NULL =>
               Sink.Set_Member (Name, Util.Beans.Objects.Null_Object, P);

            when T_NUMBER =>
               declare
                  Value : Long_Long_Integer;
               begin
                  Value := Long_Long_Integer'Value (To_String (P.Token));
                  Sink.Set_Member (Name, Util.Beans.Objects.To_Object (Value), P);
               end;

            when T_FLOAT =>
               declare
                  Value : Long_Long_Float;
               begin
                  Value := Long_Long_Float'Value (To_String (P.Token));
                  Sink.Set_Member (Name, Util.Beans.Objects.To_Object (Value), P);

               exception
                  when Constraint_Error =>
                     P.Error ("Invalid number");
               end;

            when T_STRING =>
               Sink.Set_Member (Name, Util.Beans.Objects.To_Object (P.Token), P);

            when T_TRUE =>
               Sink.Set_Member (Name, Util.Beans.Objects.To_Object (True), P);

            when T_FALSE =>
               Sink.Set_Member (Name, Util.Beans.Objects.To_Object (False), P);

            when T_EOF =>
               P.Error ("End of stream reached");
               return;

            when others =>
               P.Error ("Invalid token");
         end case;
      end Parse_Value;

      --  ------------------------------
      --  Put back a token in the buffer.
      --  ------------------------------
      procedure Put_Back (P     : in out Parser'Class;
                          Token : in Token_Type) is
      begin
         P.Pending_Token := Token;
      end Put_Back;

      --  ------------------------------
      --  Parse the expression buffer to find the next token.
      --  ------------------------------
      procedure Peek (P     : in out Parser'Class;
                      Token : out Token_Type) is
         use Ada.Characters;
         C, C1 : Character;
      begin
         --  If a token was put back, return it.
         if P.Pending_Token /= T_EOF then
            Token := P.Pending_Token;
            P.Pending_Token := T_EOF;
            return;
         end if;

         if P.Has_Pending_Char then
            C := P.Pending_Char;
         else
            --  Skip white spaces
            loop
               Stream.Read (Char => C);
               if C = Ada.Characters.Latin_1.LF then
                  P.Line_Number := P.Line_Number + 1;
               else
                  exit when C /= ' '
                    and then C /= Ada.Characters.Latin_1.CR
                    and then C /= Ada.Characters.Latin_1.HT;
               end if;
            end loop;
         end if;
         P.Has_Pending_Char := False;

         --  See what we have and continue parsing.
         case C is
            --  Literal string using double quotes
            --  Collect up to the end of the string and put
            --  the result in the parser token result.
         when '"' =>
            Delete (P.Token, 1, Length (P.Token));
            loop
               Stream.Read (Char => C1);
               if C1 = '\' then
                  Stream.Read (Char => C1);
                  case C1 is
                     when '"' | '\' | '/' =>
                        null;

                     when 'b' =>
                        C1 := Ada.Characters.Latin_1.BS;

                     when 'f' =>
                        C1 := Ada.Characters.Latin_1.VT;

                     when 'n' =>
                        C1 := Ada.Characters.Latin_1.LF;

                     when 'r' =>
                        C1 := Ada.Characters.Latin_1.CR;

                     when 't' =>
                        C1 := Ada.Characters.Latin_1.HT;

                     when 'u' =>
                        declare
                           use Interfaces;

                           C2, C3, C4 : Character;
                           Val : Interfaces.Unsigned_32;
                        begin
                           Stream.Read (Char => C1);
                           Stream.Read (Char => C2);
                           Stream.Read (Char => C3);
                           Stream.Read (Char => C4);
                           Val := Interfaces.Shift_Left (Hexdigit (C1), 12);
                           Val := Val + Interfaces.Shift_Left (Hexdigit (C2), 8);
                           Val := Val + Interfaces.Shift_Left (Hexdigit (C3), 4);
                           Val := Val + Hexdigit (C4);

                           --  Encode the value as an UTF-8 string.
                           if Val >= 16#1000# then
                              Append (P.Token, Character'Val (16#E0# or Shift_Right (Val, 12)));
                              Val := Val and 16#0fff#;
                              Append (P.Token, Character'Val (16#80# or Shift_Right (Val, 6)));
                              Val := Val and 16#03f#;
                              C1 := Character'Val (16#80# or Val);
                           elsif Val >= 16#80# then
                              Append (P.Token, Character'Val (16#C0# or Shift_Right (Val, 6)));
                              Val := Val and 16#03f#;
                              C1 := Character'Val (16#80# or Val);
                           else
                              C1 := Character'Val (Val);
                           end if;
                        end;

                     when others =>
                        P.Error ("Invalid character '" & C1 & "' in \x sequence");

                  end case;
               elsif C1 = C then
                  Token := T_STRING;
                  return;
               end if;
               Append (P.Token, C1);
            end loop;

            --  Number
         when '-' | '0' .. '9' =>
            Delete (P.Token, 1, Length (P.Token));
            Append (P.Token, C);
            Token := T_NUMBER;
            begin
               loop
                  Stream.Read (Char => C);
                  exit when C not in '0' .. '9';
                  Append (P.Token, C);
               end loop;
               if C = '.' then
                  Token := T_FLOAT;
                  Append (P.Token, C);
                  loop
                     Stream.Read (Char => C);
                     exit when C not in '0' .. '9';
                     Append (P.Token, C);
                  end loop;
               end if;
               if C = 'e' or else C = 'E' then
                  Token := T_FLOAT;
                  Append (P.Token, C);
                  Stream.Read (Char => C);
                  if C = '+' or else C = '-' then
                     Append (P.Token, C);
                     Stream.Read (Char => C);
                  end if;
                  while C in '0' .. '9' loop
                     Append (P.Token, C);
                     Stream.Read (Char => C);
                  end loop;
               end if;
               if not (C in ' ' | Latin_1.HT | Latin_1.LF | Latin_1.CR) then
                  P.Has_Pending_Char := True;
                  P.Pending_Char := C;
               end if;

            exception
               when Ada.IO_Exceptions.Data_Error =>
                  null;
            end;
            return;

            --  Parse a name composed of letters or digits.
         when 'a' .. 'z' | 'A' .. 'Z' =>
            Delete (P.Token, 1, Length (P.Token));
            Append (P.Token, C);
            loop
               Stream.Read (Char => C);
               exit when not (C in 'a' .. 'z' or else C in 'A' .. 'Z'
                              or else C in '0' .. '9' or else C = '_');
               Append (P.Token, C);
            end loop;
            --  Putback the last character unless we can ignore it.
            if not (C in ' ' | Latin_1.HT | Latin_1.LF | Latin_1.CR) then
               P.Has_Pending_Char := True;
               P.Pending_Char := C;
            end if;

            --  and empty eq false ge gt le lt ne not null true
            case Element (P.Token, 1) is
               when 'n' | 'N' =>
                  if P.Token = "null" then
                     Token := T_NULL;
                     return;
                  end if;

               when 'f' | 'F' =>
                  if P.Token = "false" then
                     Token := T_FALSE;
                     return;
                  end if;

               when 't' | 'T' =>
                  if P.Token = "true" then
                     Token := T_TRUE;
                     return;
                  end if;

               when others =>
                  null;
            end case;
            Token := T_UNKNOWN;
            return;

         when '{' =>
            Token := T_LEFT_BRACE;
            return;

         when '}' =>
            Token := T_RIGHT_BRACE;
            return;

         when '[' =>
            Token := T_LEFT_BRACKET;
            return;

         when ']' =>
            Token := T_RIGHT_BRACKET;
            return;

         when ':' =>
            Token := T_COLON;
            return;

         when ',' =>
            Token := T_COMMA;
            return;

         when others =>
            Token := T_UNKNOWN;
            return;
         end case;

      exception
         when Ada.IO_Exceptions.Data_Error =>
            Token := T_EOF;
            return;
      end Peek;

   begin
      Parse_Value (Handler, "");
   end Parse;

   --  Read a JSON file and return an object.
   function Read (Path : in String) return Util.Beans.Objects.Object is
      P : Parser;
      R : Util.Beans.Objects.Readers.Reader;
   begin
      P.Parse (Path, R);
      if P.Has_Error then
         return Util.Beans.Objects.Null_Object;
      else
         return R.Get_Root;
      end if;
   end Read;

   function Read (Content : in String) return Util.Properties.Manager is
      P : Parser;
      R : Util.Beans.Objects.Readers.Reader;
   begin
      P.Parse_String (Content, R);
      if P.Has_Error then
         return Util.Properties.To_Manager (Util.Beans.Objects.Null_Object);
      else
         return Util.Properties.To_Manager (R.Get_Root);
      end if;
   end Read;

end Util.Serialize.IO.JSON;
