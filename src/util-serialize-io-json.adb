-----------------------------------------------------------------------
--  ASF.Readers -- Print streams for servlets
--  Copyright (C) 2010 Stephane Carrez
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
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.IO_Exceptions;

with Util.Streams;
with Util.Streams.Files;
with Util.Streams.Texts;
with Ada.Streams.Stream_IO;
package body Util.Serialize.IO.JSON is

   use Ada.Strings.Unbounded;

   procedure Error (Handler  : in out Parser;
                    Message : in String) is
   begin
      raise Parse_Error with Message;
   end Error;

   procedure Parse (Handler : in out Parser;
                    Stream : in out Util.Streams.Buffered.Buffered_Stream'Class) is

      --  Put back a token in the buffer.
      procedure Put_Back (P     : in out Parser'Class;
                          Token : in Token_Type);

      --  Parse the expression buffer to find the next token.
      procedure Peek (P     : in out Parser'Class;
                      Token : out Token_Type);

      --  Parse a number
      procedure Parse_Number (P      : in out Parser'Class;
                              Result : out Long_Long_Integer);

      --  Parse a list of members
      --  members ::= pair | pair ',' members
      --  pair    ::= string ':' value
      --  value   ::= string | number | object | array | true | false | null
      procedure Parse_Pairs (P : in out Parser'Class);

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
            Peek (P, Token);

            declare
               Name : constant String := To_String (Current_Name);
            begin
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
                  --           when T_LEFT_BRACKET =>
                  --              Start_Array (P, Name);
                  --              Parse_Array (P);
                  --              Peek (P, Token);
                  --              if Token /= T_RIGHT_BRACKET then
                  --                 P.Error ("Missing ']'");
                  --              end if;
                  --              Finish_Array (P, Name);

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
            end;

            Peek (P, Token);
            if Token /= T_COMMA then
               Put_Back (P, Token);
               return;
            end if;
         end loop;
      end Parse_Pairs;

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
         use Ada.Characters.Conversions;

         C, C1 : Character;
      begin
         --  If a token was put back, return it.
         if P.Pending_Token /= T_EOF then
            Token := P.Pending_Token;
            P.Pending_Token := T_EOF;
            return;
         end if;

         --  Skip white spaces
         loop
            Stream.Read (Char => C);
            exit when C /= ' ' and C /= Ada.Characters.Latin_1.CR and C /= Ada.Characters.Latin_1.LF;
         end loop;

         --  Check for end of string.
         --           if P.Pos > P.Last then
         --              Token := T_EOF;
         --              return;
         --           end if;

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
                        null;

                     when others =>
                        null;
                  end case;
               elsif C1 = C then
                  Token := T_STRING;
                  return;
               end if;
               Append (P.Token, C1);
            end loop;
            P.Error ("Missing '""' to terminate the string");
            return;

            --  Number
         when '0' .. '9' =>

            P.Pos := P.Pos - 1;
            Parse_Number (P, P.Value);
            if P.Pos <= P.Last then
               declare
                  Decimal_Part : Long_Long_Integer := 0;
               begin
                  Stream.Read (Char => C);
                  if C = '.' then
                     if P.Pos <= P.Last then
                        Stream.Read (Char => C);
                        if C in '0' .. '9' then
                           Parse_Number (P, Decimal_Part);
                        end if;
                     end if;
                  end if;
               end;
            end if;

            Token := T_NUMBER;
            return;

            --  Parse a name composed of letters or digits.
         when 'a' .. 'z' | 'A' .. 'Z' =>
            Delete (P.Token, 1, Length (P.Token));
            Append (P.Token, C);
            while P.Pos <= P.Last loop
               Stream.Read (Char => C);
               exit when not (C in 'a' .. 'z' or C in 'A' .. 'Z'
                              or C in '0' .. '9' or C = '_');
               Append (P.Token, C);
            end loop;

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


      --  ------------------------------
      --  Parse a number
      --  ------------------------------
      procedure Parse_Number (P      : in out Parser'Class;
                              Result : out Long_Long_Integer) is
         Value : Long_Long_Integer := 0;
         Num   : Long_Long_Integer;
         C     : Character;
      begin
         while P.Pos <= P.Last loop
            Stream.Read (Char => C);
            exit when C not in '0' .. '9';
            Num := Character'Pos (C) - Character'Pos ('0');
            Value := Value * 10 + Num;
         end loop;
         Result := Value;
      end Parse_Number;

      Root_Name : constant Unbounded_String := To_Unbounded_String ("");
   begin
--      Util.Serialize.IO.Parser'Class (Parser).Start_Object (Root_Name);
      Parse (Handler);
--      Util.Serialize.IO.Parser'Class (Parser).Finish_Object (Root_Name);
   end Parse;

end Util.Serialize.IO.JSON;
