-----------------------------------------------------------------------
--  util-serialize-io-json -- JSON Serialization Driver
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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
with Ada.IO_Exceptions;

with Util.Strings;
with Util.Streams;
with Util.Streams.Buffered;
package body Util.Serialize.IO.JSON is

   use Ada.Strings.Unbounded;

   --  -----------------------
   --  Start a JSON document.  This operation writes the initial JSON marker ('{').
   --  -----------------------
   procedure Start_Document (Stream : in out Output_Stream) is
   begin
      Stream.Write ('{');
   end Start_Document;

   --  -----------------------
   --  Finish a JSON document by writing the final JSON marker ('}').
   --  -----------------------
   procedure End_Document (Stream : in out Output_Stream) is
   begin
      Stream.Write ('}');
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
                     Util.Streams.Texts.TR.To_Hex (Streams.Buffered.Buffered_Stream (Stream), C);

               end case;
            end if;
         end;
      end loop;
      Stream.Write ('"');
   end Write_String;

   --  -----------------------
   --  Start writing an object identified by the given name
   --  -----------------------
   procedure Start_Entity (Stream : in out Output_Stream;
                           Name   : in String) is
      Current : access Node_Info := Node_Info_Stack.Current (Stream.Stack);
   begin
      if Current /= null then
         if Current.Has_Fields then
            Stream.Write (',');
         else
            Current.Has_Fields := True;
         end if;
      end if;
      Node_Info_Stack.Push (Stream.Stack);
      Current := Node_Info_Stack.Current (Stream.Stack);
      Current.Has_Fields := False;

      if Name'Length > 0 then
         Stream.Write_String (Name);
         Stream.Write (':');
      end if;
      Stream.Write ('{');
   end Start_Entity;

   --  -----------------------
   --  Finish writing an object identified by the given name
   --  -----------------------
   procedure End_Entity (Stream : in out Output_Stream;
                         Name   : in String) is
      pragma Unreferenced (Name);
   begin
      Node_Info_Stack.Pop (Stream.Stack);
      Stream.Write ('}');
   end End_Entity;

   --  -----------------------
   --  Write an attribute member from the current object
   --  -----------------------
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;

      Current : constant access Node_Info := Node_Info_Stack.Current (Stream.Stack);
   begin
      if Current /= null then
         if Current.Has_Fields then
            Stream.Write (",");
         else
            Current.Has_Fields := True;
         end if;
      end if;
      Stream.Write_String (Name);
      Stream.Write (':');
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
            Stream.Write (Util.Beans.Objects.To_Long_Long_Integer (Value));

         when others =>
            Stream.Write_String (Util.Beans.Objects.To_String (Value));

      end case;
   end Write_Attribute;

   --  -----------------------
   --  Write an object value as an entity
   --  -----------------------
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
   begin
      Stream.Write_Attribute (Name, Value);
   end Write_Entity;

   --  -----------------------
   --  Start an array that will contain the specified number of elements
   --  Example:  "list": [
   --  -----------------------
   procedure Start_Array (Stream : in out Output_Stream;
                          Name   : in String;
                          Length : in Ada.Containers.Count_Type) is
      pragma Unreferenced (Length);

      Current : constant access Node_Info := Node_Info_Stack.Current (Stream.Stack);
   begin
      if Current /= null then
         if Current.Has_Fields then
            Stream.Write (',');
         else
            Current.Has_Fields := True;
         end if;
      end if;
      Node_Info_Stack.Push (Stream.Stack);
      Stream.Write_String (Name);
      Stream.Write (':');
      Stream.Write ('[');
   end Start_Array;

   --  -----------------------
   --  Finishes an array
   --  -----------------------
   procedure End_Array (Stream : in out Output_Stream) is
   begin
      Node_Info_Stack.Pop (Stream.Stack);
      Stream.Write (']');
   end End_Array;

   --  -----------------------
   --  Get the current location (file and line) to report an error message.
   --  -----------------------
   function Get_Location (Handler : in Parser) return String is
   begin
      return Util.Strings.Image (Handler.Line_Number);
   end Get_Location;

   procedure Parse (Handler : in out Parser;
                    Stream : in out Util.Streams.Buffered.Buffered_Stream'Class) is

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

      procedure Parse (P : in out Parser'Class);

      procedure Parse (P : in out Parser'Class) is
         Token : Token_Type;
      begin
         Peek (P, Token);
         if Token /= T_LEFT_BRACE then
            P.Error ("Missing '{'");
         end if;
         Parse_Pairs (P);
         Peek (P, Token);
         if Token /= T_RIGHT_BRACE then
            P.Error ("Missing '}'");
         end if;
      end Parse;

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
         if C >= '0' and C <= '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif C >= 'a' and C <= 'f' then
            return Character'Pos (C) - Character'Pos ('a') + 10;
         elsif C >= 'A' and C <= 'F' then
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
      begin
         Peek (P, Token);
         case Token is
            when T_LEFT_BRACE =>
               P.Start_Object (Name);
               Parse_Pairs (P);
               Peek (P, Token);
               if Token /= T_RIGHT_BRACE then
                  P.Error ("Missing '}'");
               end if;
               P.Finish_Object (Name);

                  --
            when T_LEFT_BRACKET =>
               P.Start_Array (Name);
               Peek (P, Token);
               if Token /= T_RIGHT_BRACKET then
                  Put_Back (P, Token);
                  loop
                     Parse_Value (P, Name);
                     Peek (P, Token);
                     exit when Token = T_RIGHT_BRACKET;
                     if Token /= T_COMMA then
                        P.Error ("Missing ']'");
                     end if;
                  end loop;
               end if;
               P.Finish_Array (Name);

            when T_NULL =>
               P.Set_Member (Name, Util.Beans.Objects.Null_Object);

            when T_NUMBER =>
               P.Set_Member (Name, Util.Beans.Objects.To_Object (P.Token));

            when T_STRING =>
               P.Set_Member (Name, Util.Beans.Objects.To_Object (P.Token));

            when T_TRUE =>
               P.Set_Member (Name, Util.Beans.Objects.To_Object (True));

            when T_FALSE =>
               P.Set_Member (Name, Util.Beans.Objects.To_Object (False));

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
                    and C /= Ada.Characters.Latin_1.CR and C /= Ada.Characters.Latin_1.HT;
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
            loop
               Stream.Read (Char => C);
               exit when C not in '0' .. '9';
               Append (P.Token, C);
            end loop;
            if C = '.' then
               Append (P.Token, C);
               loop
                  Stream.Read (Char => C);
                  exit when C not in '0' .. '9';
                  Append (P.Token, C);
               end loop;
            end if;
            if C = 'e' or C = 'E' then
               Append (P.Token, C);
               Stream.Read (Char => C);
               if C = '+' or C = '-' then
                  Append (P.Token, C);
                  Stream.Read (Char => C);
               end if;
               loop
                  Stream.Read (Char => C);
                  exit when C not in '0' .. '9';
                  Append (P.Token, C);
               end loop;
            end if;
            if C /= ' ' and C /= Ada.Characters.Latin_1.HT then
               P.Has_Pending_Char := True;
               P.Pending_Char := C;
            end if;
            Token := T_NUMBER;
            return;

            --  Parse a name composed of letters or digits.
         when 'a' .. 'z' | 'A' .. 'Z' =>
            Delete (P.Token, 1, Length (P.Token));
            Append (P.Token, C);
            loop
               Stream.Read (Char => C);
               exit when not (C in 'a' .. 'z' or C in 'A' .. 'Z'
                              or C in '0' .. '9' or C = '_');
               Append (P.Token, C);
            end loop;
            --  Putback the last character unless we can ignore it.
            if C /= ' ' and C /= Ada.Characters.Latin_1.HT then
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
      Parser'Class (Handler).Start_Object ("");
      Parse (Handler);
      Parser'Class (Handler).Finish_Object ("");
   end Parse;

end Util.Serialize.IO.JSON;
