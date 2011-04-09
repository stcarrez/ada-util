-----------------------------------------------------------------------
--  util-serialize-io -- IO Drivers for serialization
--  Copyright (C) 2010, 2011 Stephane Carrez
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
with Util.Beans.Objects;
with Util.Streams;
with Util.Streams.Buffered;
with Util.Serialize.Contexts;
with Util.Serialize.Mappers;
with Ada.Containers;
with Util.Stacks;
package Util.Serialize.IO is

   Parse_Error : exception;

   --  ------------------------------
   --  Output stream for serialization
   --  ------------------------------
   --  The <b>Output_Stream</b> interface defines the abstract operations for
   --  the serialization framework to write objects on the stream according to
   --  a target format such as XML or JSON.
   type Output_Stream is limited interface and Util.Streams.Output_Stream;

   procedure Start_Entity (Stream : in out Output_Stream;
                           Name   : in String) is null;

   procedure End_Entity (Stream : in out Output_Stream;
                         Name   : in String) is null;

   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is abstract;

   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is abstract;

   procedure Start_Array (Stream : in out Output_Stream;
                          Length : in Ada.Containers.Count_Type) is null;

   procedure End_Array (Stream : in out Output_Stream) is null;

   type Parser is abstract new Util.Serialize.Contexts.Context with private;

   --  Parse the stream using the JSON parser.
   procedure Parse (Handler : in out Parser;
                    Stream  : in out Util.Streams.Buffered.Buffered_Stream'Class) is abstract;

   --  Read the file and parse it using the parser.
   procedure Parse (Handler : in out Parser;
                    File    : in String);

   --  Parse the content string.
   procedure Parse_String (Handler : in out Parser;
                           Content : in String);

   --  Returns true if the <b>Parse</b> operation detected at least one error.
   function Has_Error (Handler : in Parser) return Boolean;

   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
   procedure Start_Object (Handler : in out Parser;
                           Name    : in String);

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
   procedure Finish_Object (Handler : in out Parser;
                            Name    : in String);

   procedure Start_Array (Handler : in out Parser;
                          Name    : in String);

   procedure Finish_Array (Handler : in out Parser;
                           Name    : in String);

   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   procedure Set_Member (Handler : in out Parser;
                         Name    : in String;
                         Value   : in Util.Beans.Objects.Object);

   --  Report an error while parsing the JSON stream.
   procedure Error (Handler : in out Parser;
                    Message : in String);

   procedure Add_Mapping (Handler : in out Parser;
                          Path    : in String;
                          Mapper  : in Util.Serialize.Mappers.Mapper_Access);

private

   --  Implementation limitation:  the max number of active mapping nodes
   MAX_NODES : constant Positive := 10;

   type Mapper_Access_Array is array (1 .. MAX_NODES) of Serialize.Mappers.Mapper_Access;

   procedure Push (Handler : in out Parser);

   --  Pop the context and restore the previous context when leaving an element
   procedure Pop (Handler  : in out Parser);

   function Find_Mapper (Handler : in Parser;
                         Name    : in String) return Util.Serialize.Mappers.Mapper_Access;

   type Element_Context is record
      --  The object mapper being process.
      Object_Mapper : Util.Serialize.Mappers.Mapper_Access;

      --  The current inner mapper within the object mapper.
--        Mapper        : Util.Serialize.Mappers.Mapper_Access;

      --  The active mapping nodes.
      Active_Nodes : Mapper_Access_Array;
   end record;
   type Element_Context_Access is access all Element_Context;

   package Context_Stack is new Util.Stacks (Element_Type => Element_Context,
                                             Element_Type_Access => Element_Context_Access);

   type Parser is abstract new Util.Serialize.Contexts.Context with record
      Error_Flag     : Boolean := False;
      Stack          : Context_Stack.Stack;
      Mapping_Tree   : aliased Mappers.Mapper;
      Current_Mapper : Util.Serialize.Mappers.Mapper_Access;
   end record;

end Util.Serialize.IO;
