-----------------------------------------------------------------------
--  Util-texts -- Various Text Transformation Utilities
--  Copyright (C) 2001, 2002, 2003, 2009, 2010 Stephane Carrez
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
package body Util.Texts.Transforms is

   type Code is mod 2**32;

   Conversion : constant String (1 .. 16) := "0123456789ABCDEF";

   procedure Put_Dec (Into  : in out Stream;
                      Value : in Code);

   procedure To_Hex (Into  : in out Stream;
                     Value : in Code);

   procedure Put (Into  : in out Stream;
                  Value : in String) is
   begin
      for I in Value'Range loop
         Put (Into, Value (I));
      end loop;
   end Put;

   --  ------------------------------
   --  Write in the output stream the value as a \uNNNN encoding form.
   --  ------------------------------
   procedure To_Hex (Into  : in out Stream;
                     Value : in Char) is
   begin
      To_Hex (Into, Code (Char'Pos (Value)));
   end To_Hex;

   --  ------------------------------
   --  Write in the output stream the value as a \uNNNN encoding form.
   --  ------------------------------
   procedure To_Hex (Into  : in out Stream;
                     Value : in Code) is
      S          : String (1 .. 6) := (1 => '\', 2 => 'u', others => '0');
      P          : Code := Value;
      N          : Code;
      I          : Positive := S'Last;
   begin
      while P /= 0 loop
         N := P mod 16;
         P := P / 16;
         S (I) := Conversion (Positive'Val (N + 1));
         exit when I = 1;
         I := I - 1;
      end loop;
      Put (Into, S);
   end To_Hex;

   procedure Put_Dec (Into  : in out Stream;
                      Value : in Code) is
      S : String (1 .. 9) := (others => '0');
      P : Code := Value;
      N : Code;
      I : Positive := S'Last;
   begin
      while P /= 0 loop
         N := P mod 10;
         P := P / 10;
         S (I) := Conversion (Positive'Val (N + 1));
         exit when P = 0;
         I := I - 1;
      end loop;
      Put (Into, S (I .. S'Last));
   end Put_Dec;

   --  ------------------------------
   --  Capitalize the string into the result stream.
   --  ------------------------------
   procedure Capitalize (Content : in Input;
                         Into    : in out Stream) is
      Upper  : Boolean := True;
      C      : Code;
   begin
      for I in Content'Range loop
         if Upper then
            C := Char'Pos (To_Upper (Content (I)));
            Upper := False;
         else
            C := Char'Pos (To_Lower (Content (I)));
            if C = Character'Pos ('_') or C = Character'Pos ('.') or C = Character'Pos (':')
              or C = Character'Pos (';') or C = Character'Pos (',') or C = Character'Pos (' ') then
               Upper := True;
            end if;
         end if;
         Put (Into, Character'Val (C));
      end loop;
   end Capitalize;

   --  ------------------------------
   --  Capitalize the string
   --  ------------------------------
   function Capitalize (Content : Input) return Input is
      Result : Stream;
   begin
      Capitalize (Content, Result);
      return To_Input (Result);
   end Capitalize;

   --  ------------------------------
   --  Translate the input string into an upper case string in the result stream.
   --  ------------------------------
   procedure To_Upper_Case (Content : in Input;
                            Into    : in out Stream) is
      C      : Code;
   begin
      for I in Content'Range loop
         C := Char'Pos (To_Upper (Content (I)));
         Put (Into, Character'Val (C));
      end loop;
   end To_Upper_Case;

   --  ------------------------------
   --  Translate the input string into an upper case string.
   --  ------------------------------
   function To_Upper_Case (Content : Input) return Input is
      Result : Input (Content'Range);
   begin
      for I in Content'Range loop
         Result (I) := To_Upper (Content (I));
      end loop;
      return Result;
   end To_Upper_Case;

   --  ------------------------------
   --  Translate the input string into a lower case string in the result stream.
   --  ------------------------------
   procedure To_Lower_Case (Content : in Input;
                            Into    : in out Stream) is
      C      : Code;
   begin
      for I in Content'Range loop
         C := Char'Pos (To_Lower (Content (I)));
         Put (Into, Character'Val (C));
      end loop;
   end To_Lower_Case;

   --  ------------------------------
   --  Translate the input string into a lower case string in the result stream.
   --  ------------------------------
   function To_Lower_Case (Content : Input) return Input is
      Result : Input (Content'Range);
   begin
      for I in Content'Range loop
         Result (I) := To_Lower (Content (I));
      end loop;
      return Result;
   end To_Lower_Case;

   procedure Escape_Java_Script (Content : in Input;
                                 Into    : in out Stream) is
   begin
      Escape_Java (Content             => Content, Into => Into,
                   Escape_Single_Quote => True);
   end Escape_Java_Script;

   function Escape_Java_Script (Content : Input) return Input is
      Result : Stream;
   begin
      Escape_Java (Content             => Content,
                   Into                => Result,
                   Escape_Single_Quote => True);
      return To_Input (Result);
   end Escape_Java_Script;

   procedure Escape_Java (Content : in Input;
                          Into    : in out Stream) is
   begin
      Escape_Java (Content             => Content, Into => Into,
                   Escape_Single_Quote => False);
   end Escape_Java;

   function Escape_Java (Content : Input) return Input is
      Result : Stream;
   begin
      Escape_Java (Content             => Content,
                   Into                => Result,
                   Escape_Single_Quote => False);
      return To_Input (Result);
   end Escape_Java;

   procedure Escape_Java (Content             : in Input;
                          Escape_Single_Quote : in Boolean;
                          Into                : in out Stream) is
      C : Code;
   begin
      for I in Content'Range loop
         C := Char'Pos (Content (I));
         if C < 16#20# then
            if C = 16#0A# then
               Put (Into, '\');
               Put (Into, 'n');

            elsif C = 16#0D# then
               Put (Into, '\');
               Put (Into, 'r');

            elsif C = 16#08# then
               Put (Into, '\');
               Put (Into, 'b');

            elsif C = 16#09# then
               Put (Into, '\');
               Put (Into, 't');

            elsif C = 16#0C# then
               Put (Into, '\');
               Put (Into, 'f');
            else
               To_Hex (Into, C);
            end if;

         elsif C = 16#27# then
            if Escape_Single_Quote then
               Put (Into, '\');
            end if;
            Put (Into, Character'Val (C));

         elsif C = 16#22# then
            Put (Into, '\');
            Put (Into, Character'Val (C));

         elsif C = 16#5C# then
            Put (Into, '\');
            Put (Into, Character'Val (C));

         elsif C > 16#80# then
            To_Hex (Into, C);

         else
            Put (Into, Character'Val (C));
         end if;
      end loop;
   end Escape_Java;


   function Escape_Xml (Content : Input) return Input is
      Result : Stream;
   begin
      Escape_Xml (Content => Content,
                  Into    => Result);
      return To_Input (Result);
   end Escape_Xml;

   --  Escape the content into the result stream using the XML
   --  escape rules:
   --   '<' -> '&lt;'
   --   '>' -> '&gt;'
   --   ''' -> '&apos;'
   --   '&' -> '&amp;'
   --       -> '&#nnn;' if Character'Pos >= 128
   procedure Escape_Xml (Content : in Input;
                         Into    : in out Stream) is
      C : Code;
   begin
      for I in Content'Range loop
         C := Char'Pos (Content (I));
         if C = Character'Pos ('<') then
            Put (Into, "&lt;");

         elsif C = Character'Pos ('>') then
            Put (Into, "&gt;");

         elsif C = Character'Pos ('&') then
            Put (Into, "&amp;");

         elsif C = Character'Pos (''') then
            Put (Into, "&apos;");

         elsif C > 16#7F# then
            Put (Into, '&');
            Put (Into, '#');
            Put_Dec (Into, C);
            Put (Into, ';');

         else
            Put (Into, Character'Val (C));
         end if;
      end loop;
   end Escape_Xml;

end Util.Texts.Transforms;
