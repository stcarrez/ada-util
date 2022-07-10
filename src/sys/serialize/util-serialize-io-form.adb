-----------------------------------------------------------------------
--  swagger-streams-forms -- x-www-form-urlencoded streams
--  Copyright (C) 2018, 2022 Stephane Carrez
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
with Ada.IO_Exceptions;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Util.Strings;
with Util.Beans.Objects.Readers;
package body Util.Serialize.IO.Form is

   use Ada.Strings.Unbounded;

   procedure Initialize (Stream : in out Output_Stream;
                         Output : in Util.Streams.Texts.Print_Stream_Access) is
   begin
      Stream.Stream := Output;
   end Initialize;

   --  ------------------------------
   --  Flush the buffer (if any) to the sink.
   --  ------------------------------
   overriding
   procedure Flush (Stream : in out Output_Stream) is
   begin
      Stream.Stream.Flush;
   end Flush;

   --  ------------------------------
   --  Close the sink.
   --  ------------------------------
   overriding
   procedure Close (Stream : in out Output_Stream) is
   begin
      Stream.Stream.Close;
   end Close;

   --  ------------------------------
   --  Write the buffer array to the output stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out Output_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      Stream.Stream.Write (Buffer);
   end Write;

   Conversion : constant String (1 .. 16) := "0123456789ABCDEF";

   procedure Write_Escape (Stream : in out Output_Stream;
                           Value  : in String) is
   begin
      for C of Value loop
         if C = ' ' then
            Stream.Stream.Write ('+');
         elsif C >= 'a' and then C <= 'z' then
            Stream.Stream.Write (C);
         elsif C >= 'A' and then C <= 'Z' then
            Stream.Stream.Write (C);
         elsif C >= '0' and then C <= '9' then
            Stream.Stream.Write (C);
         elsif C = '_' or else C = '-' then
            Stream.Stream.Write (C);
         else
            Stream.Stream.Write ('%');
            Stream.Stream.Write (Conversion (1 + (Character'Pos (C) / 16)));
            Stream.Stream.Write (Conversion (1 + (Character'Pos (C) mod 16)));
         end if;
      end loop;
   end Write_Escape;

   --  Write the attribute name/value pair.
   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in String) is
   begin
      if Stream.Has_Param then
         Stream.Stream.Write ('&');
      end if;
      Stream.Has_Param := True;
      Stream.Write_Escape (Name);
      Stream.Stream.Write ('=');
      Stream.Write_Escape (Value);
   end Write_Attribute;

   overriding
   procedure Write_Wide_Attribute (Stream : in out Output_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String) is
   begin
      if Stream.Has_Param then
         Stream.Stream.Write ('&');
      end if;
      Stream.Has_Param := True;
      Stream.Write_Escape (Name);
      Stream.Stream.Write ('=');
      Stream.Write_Escape (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Value));
   end Write_Wide_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Integer) is
   begin
      if Stream.Has_Param then
         Stream.Stream.Write ('&');
      end if;
      Stream.Has_Param := True;
      Stream.Write_Escape (Name);
      Stream.Stream.Write ('=');
      Stream.Stream.Write (Util.Strings.Image (Value));
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Boolean) is
   begin
      if Stream.Has_Param then
         Stream.Stream.Write ('&');
      end if;
      Stream.Has_Param := True;
      Stream.Write_Escape (Name);
      Stream.Stream.Write ('=');
      Stream.Stream.Write (if Value then "true" else "false");
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is
   begin
      null;
   end Write_Attribute;

   --  Write the attribute with a null value.
   overriding
   procedure Write_Null_Attribute (Stream : in out Output_Stream;
                                   Name   : in String) is
   begin
      null;
   end Write_Null_Attribute;

   --  Write the entity value.
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

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Boolean) is
   begin
      Stream.Write_Attribute (Name, Value);
   end Write_Entity;

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
      null;
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
      null;
   end Write_Enum_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
   begin
      null;
   end Write_Entity;

   --  Write an entity with a null value.
   overriding
   procedure Write_Null_Entity (Stream : in out Output_Stream;
                                Name   : in String) is
   begin
      null;
   end Write_Null_Entity;

   --  ------------------------------
   --  Parse the stream using the form parser.
   --  ------------------------------
   overriding
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Input_Buffer_Stream'Class;
                    Sink    : in out Reader'Class) is
      function From_Hex (C : in Character) return Natural;
      procedure Parse_Token (Into : out Ada.Strings.Unbounded.Unbounded_String);

      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Value : Ada.Strings.Unbounded.Unbounded_String;
      Last  : Character;

      function From_Hex (C : in Character) return Natural is
      begin
         if C >= 'a' and then C <= 'f' then
            return Character'Pos (C) - Character'Pos ('a') + 10;
         elsif C >= 'A' and then C <= 'F' then
            return Character'Pos (C) - Character'Pos ('A') + 10;
         elsif C >= '0' and then C <= '9' then
            return Character'Pos (C) - Character'Pos ('0');
         else
            raise Parse_Error with "Invalid hexadecimal character: " & C;
         end if;
      end From_Hex;

      procedure Parse_Token (Into : out Ada.Strings.Unbounded.Unbounded_String) is
         C1, C2 : Character;
      begin
         Into := Ada.Strings.Unbounded.Null_Unbounded_String;
         loop
            Stream.Read (C1);
            if C1 = '&' or else C1 = '=' then
               Last := C1;
               return;
            end if;
            if C1 = '+' then
               Ada.Strings.Unbounded.Append (Into, ' ');
            elsif C1 = '%' then
               Stream.Read (C1);
               Stream.Read (C2);
               Ada.Strings.Unbounded.Append (Into,
                                             Character'Val ((16 * From_Hex (C1)) + From_Hex (C2)));
            else
               Ada.Strings.Unbounded.Append (Into, C1);
            end if;
         end loop;

      exception
         when Ada.IO_Exceptions.Data_Error =>
            return;
      end Parse_Token;

   begin
      Sink.Start_Object ("", Handler);
      loop
         Parse_Token (Name);
         if Last /= '=' then
            raise Parse_Error with "Missing '=' after parameter name";
         end if;
         Parse_Token (Value);
         Sink.Set_Member (To_String (Name), Util.Beans.Objects.To_Object (Value), Handler);
         if Stream.Is_Eof then
            Sink.Finish_Object ("", Handler);
            return;
         end if;
         if Last /= '&' then
            raise Parse_Error with "Missing '&' after parameter value";
         end if;
      end loop;
   end Parse;

   --  ------------------------------
   --  Get the current location (file and line) to report an error message.
   --  ------------------------------
   overriding
   function Get_Location (Handler : in Parser) return String is
      pragma Unreferenced (Handler);
   begin
      return "";
   end Get_Location;

   --  ------------------------------
   --  Read a form file and return an object.
   --  ------------------------------
   function Read (Path : in String) return Util.Beans.Objects.Object is
      P : Parser;
      R : Util.Beans.Objects.Readers.Reader;
   begin
      P.Parse (Path, R);
      return R.Get_Root;
   end Read;

end Util.Serialize.IO.Form;
