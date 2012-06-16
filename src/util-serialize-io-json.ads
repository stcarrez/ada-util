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
with Ada.Strings.Unbounded;
with Util.Streams.Texts;
with Util.Stacks;
package Util.Serialize.IO.JSON is

   --  ------------------------------
   --  JSON Output Stream
   --  ------------------------------
   --  The <b>Output_Stream</b> provides methods for creating a JSON output stream.
   --  The stream object takes care of the JSON escape rules.
   type Output_Stream is
     new Util.Streams.Texts.Print_Stream and Util.Serialize.IO.Output_Stream with private;

   --  Start a JSON document.  This operation writes the initial JSON marker ('{').
   procedure Start_Document (Stream : in out Output_Stream);

   --  Finish a JSON document by writing the final JSON marker ('}').
   procedure End_Document (Stream : in out Output_Stream);

   --  Write the value as a JSON string.  Special characters are escaped using the JSON
   --  escape rules.
   procedure Write_String (Stream : in out Output_Stream;
                           Value  : in String);

   --  Start a new JSON object.  If the name is not empty, write it as a string
   --  followed by the ':' (colon).  The JSON object starts with '{' (curly brace).
   --  Example:   "list": {
   procedure Start_Entity (Stream : in out Output_Stream;
                           Name   : in String);

   --  Terminates the current JSON object.
   procedure End_Entity (Stream : in out Output_Stream;
                         Name   : in String);

   --  Write a JSON name/value pair.  The value is written according to its type
   --  Example:  "name": null
   --            "name": false
   --            "name": 12
   --            "name": "value"
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object);

   --  Write a JSON name/value pair (see Write_Attribute).
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object);

   --  Starts a JSON array.
   --  Example:  "list": [
   procedure Start_Array (Stream : in out Output_Stream;
                          Name   : in String;
                          Length : in Ada.Containers.Count_Type);

   --  Terminates a JSON array.
   procedure End_Array (Stream : in out Output_Stream);

   type Parser is new Serialize.IO.Parser with private;

   --  Parse the stream using the JSON parser.
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Buffered_Stream'Class);

   --  Get the current location (file and line) to report an error message.
   function Get_Location (Handler : in Parser) return String;

private

   type Node_Info is record
      Is_Array   : Boolean := False;
      Has_Fields : Boolean := False;
   end record;
   type Node_Info_Access is access all Node_Info;

   package Node_Info_Stack is new Util.Stacks (Element_Type => Node_Info,
                                               Element_Type_Access => Node_Info_Access);

   type Output_Stream is
     new Util.Streams.Texts.Print_Stream and Util.Serialize.IO.Output_Stream with record
      Stack : Node_Info_Stack.Stack;
   end record;

   type Token_Type is (T_EOF, T_LEFT_BRACE, T_RIGHT_BRACE, T_LEFT_BRACKET,
                       T_RIGHT_BRACKET, T_COLON, T_COMMA, T_TRUE, T_FALSE,
                       T_STRING, T_NUMBER, T_BOOLEAN, T_UNKNOWN, T_NULL);

   type Parser is new Util.Serialize.IO.Parser with record
      Token            : Ada.Strings.Unbounded.Unbounded_String;
      Pending_Token    : Token_Type := T_EOF;
      Line_Number      : Natural  := 1;
      Has_Pending_Char : Boolean := False;
      Pending_Char     : Character;
   end record;

end Util.Serialize.IO.JSON;
