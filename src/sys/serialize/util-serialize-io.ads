-----------------------------------------------------------------------
--  util-serialize-io -- IO Drivers for serialization
--  Copyright (C) 2010, 2011, 2016, 2017, 2022 Stephane Carrez
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
with Ada.Calendar;

with Util.Beans.Objects;
with Util.Streams;
with Util.Streams.Buffered;
with Util.Log.Loggers;
with Util.Nullables;
package Util.Serialize.IO is

   Parse_Error : exception;

   --  ------------------------------
   --  Output stream for serialization
   --  ------------------------------
   --  The <b>Output_Stream</b> interface defines the abstract operations for
   --  the serialization framework to write objects on the stream according to
   --  a target format such as XML or JSON.
   type Output_Stream is limited interface and Util.Streams.Output_Stream;

   --  Start a document.
   procedure Start_Document (Stream : in out Output_Stream) is null;

   --  Finish a document.
   procedure End_Document (Stream : in out Output_Stream) is null;

   procedure Start_Entity (Stream : in out Output_Stream;
                           Name   : in String) is null;

   procedure End_Entity (Stream : in out Output_Stream;
                         Name   : in String) is null;

   --  Write the attribute name/value pair.
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in String) is abstract;

   procedure Write_Wide_Attribute (Stream : in out Output_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String) is abstract;

   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Integer) is abstract;

   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Boolean) is abstract;

   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is abstract;

   --  Write the attribute with a null value.
   procedure Write_Null_Attribute (Stream : in out Output_Stream;
                                   Name   : in String) is abstract;

   procedure Write_Attribute (Stream : in out Output_Stream'Class;
                              Name   : in String;
                              Value  : in Ada.Strings.Unbounded.Unbounded_String);

   --  Write the entity value.
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in String) is abstract;

   procedure Write_Wide_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String) is abstract;

   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Boolean) is abstract;

   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Integer) is abstract;

   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Ada.Calendar.Time) is abstract;

   procedure Write_Long_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Integer) is abstract;

   procedure Write_Enum_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in String) is abstract;

   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is abstract;

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Ada.Strings.Unbounded.Unbounded_String);

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_String);

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_Time);

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_Boolean);

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_Integer);

   procedure Write_Entity (Stream : in out Output_Stream'Class;
                           Name   : in String;
                           Value  : in Util.Nullables.Nullable_Long);

   --  Write an entity with a null value.
   procedure Write_Null_Entity (Stream : in out Output_Stream;
                                Name   : in String) is abstract;

   procedure Start_Array (Stream : in out Output_Stream;
                          Name   : in String) is null;

   procedure End_Array (Stream : in out Output_Stream;
                        Name   : in String) is null;

   type Reader is limited interface;

   --  Start a document.
   procedure Start_Document (Stream : in out Reader) is null;

   --  Finish a document.
   procedure End_Document (Stream : in out Reader) is null;

   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   procedure Start_Object (Handler : in out Reader;
                           Name    : in String;
                           Logger  : in out Util.Log.Logging'Class) is abstract;

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   procedure Finish_Object (Handler : in out Reader;
                            Name    : in String;
                            Logger  : in out Util.Log.Logging'Class) is abstract;

   procedure Start_Array (Handler : in out Reader;
                          Name    : in String;
                          Logger  : in out Util.Log.Logging'Class) is abstract;

   procedure Finish_Array (Handler : in out Reader;
                           Name    : in String;
                           Count   : in Natural;
                           Logger  : in out Util.Log.Logging'Class) is abstract;

   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   procedure Set_Member (Handler   : in out Reader;
                         Name      : in String;
                         Value     : in Util.Beans.Objects.Object;
                         Logger    : in out Util.Log.Logging'Class;
                         Attribute : in Boolean := False) is abstract;

   type Parser is abstract new Util.Log.Logging with private;

   --  Parse the stream using the JSON parser.
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Input_Buffer_Stream'Class;
                    Sink    : in out Reader'Class) is abstract;

   --  Read the file and parse it using the parser.
   procedure Parse (Handler : in out Parser;
                    File    : in String;
                    Sink    : in out Reader'Class);

   --  Parse the content string.
   procedure Parse_String (Handler : in out Parser;
                           Content : in String;
                           Sink    : in out Reader'Class);

   --  Returns true if the <b>Parse</b> operation detected at least one error.
   function Has_Error (Handler : in Parser) return Boolean;

   --  Set the error logger to report messages while parsing and reading the input file.
   procedure Set_Logger (Handler : in out Parser;
                         Logger  : in Util.Log.Loggers.Logger_Access);

   --  Get the current location (file and line) to report an error message.
   function Get_Location (Handler : in Parser) return String;

   --  Report an error while parsing the input stream.  The error message will be reported
   --  on the logger associated with the parser.  The parser will be set as in error so that
   --  the <b>Has_Error</b> function will return True after parsing the whole file.
   overriding
   procedure Error (Handler : in out Parser;
                    Message : in String);

private

   type Parser is abstract new Util.Log.Logging with record
      Error_Flag     : Boolean := False;

      --  The file name to use when reporting errors.
      File           : Ada.Strings.Unbounded.Unbounded_String;

      --  The logger which is used to report error messages when parsing an input file.
      Error_Logger   : Util.Log.Loggers.Logger_Access := null;
   end record;

end Util.Serialize.IO;
