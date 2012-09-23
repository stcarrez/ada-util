-----------------------------------------------------------------------
--  util-serialize-mappers -- Serialize objects in various formats
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
with Ada.Strings.Unbounded;
with Ada.Finalization;

with Util.Beans.Objects;
with Util.Log.Loggers;
with Util.Serialize.Contexts;
package Util.Serialize.Mappers is

   Mapping_Error : exception;

   --  The <b>Field_Error</b> exception can be raised by a mapper to indicate that the field
   --  that was extracted is invalid.  The exception message will be reported as an error message
   --  and the IO reader will be marked as in error.  The IO reader will continue to read and
   --  process the mappings.
   Field_Error   : exception;

   --  The <b>Field_Fatal_Error</b> exception is similar to the <b>Field_Error</b> exception.
   --  However the IO reader will report the error and immediately stop.
   Field_Fatal_Error   : exception;

   --  ------------------------------
   --  Mapping
   --  ------------------------------
   --  The <b>Mapping</b> represents a rule component to establish a mapping
   --  when reading some format (XML, JSON).
   type Mapping is abstract tagged limited private;
   type Mapping_Access is access all Mapping'Class;

   --  ------------------------------
   --  Mapper
   --  ------------------------------
   --  The <b>Mapper</b> represents a node of the mapping tree.  The mapping
   --  tree is walked horizontally to find siblings.  It is walked vertically when
   --  entering or leaving an object.
   type Mapper is new Ada.Finalization.Limited_Controlled with private;
   type Mapper_Access is access all Mapper'Class;

   --  Execute the mapping operation on the object associated with the current context.
   --  The object is extracted from the context and the <b>Execute</b> operation is called.
   procedure Execute (Handler : in Mapper;
                      Map     : in Mapping'Class;
                      Ctx     : in out Util.Serialize.Contexts.Context'Class;
                      Value   : in Util.Beans.Objects.Object);

   --  Add a mapping for setting a member.  When the attribute rule defined by <b>Path</b>
   --  is matched, the <b>Set_Member</b> procedure will be called with the value and the
   --  <b>Field</b> identification.
   --  Example:
   --     info/first_name    matches:  <info><first_name>...</first_name></info>
   --     info/a/b/name      matches:  <info><a><b><name>...</name></b></a></info>
   --     */a/b/name         matches:  <i><i><j><a><b><name>...</name></b></a></j></i></i>
   --     **/name            matches:  <i><name>...</name></i>, <b><c><name>...</name></c></b>
   procedure Add_Mapping (Into : in out Mapper;
                          Path : in String;
                          Map  : in Mapping_Access);

   procedure Add_Mapping (Into : in out Mapper;
                          Path : in String;
                          Map  : in Mapper_Access);

   --  Clone the <b>Handler</b> instance and get a copy of that single object.
   function Clone (Handler : in Mapper) return Mapper_Access;

   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   procedure Set_Member (Handler   : in Mapper;
                         Name      : in String;
                         Value     : in Util.Beans.Objects.Object;
                         Attribute : in Boolean := False;
                         Context   : in out Util.Serialize.Contexts.Context'Class);

   procedure Start_Object (Handler : in Mapper;
                           Context : in out Util.Serialize.Contexts.Context'Class;
                           Name    : in String);

   procedure Finish_Object (Handler : in Mapper;
                            Context : in out Util.Serialize.Contexts.Context'Class;
                            Name    : in String);

   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   function Find_Mapper (Controller : in Mapper;
                         Name       : in String;
                         Attribute  : in Boolean := False) return Mapper_Access;


   function Is_Proxy (Controller : in Mapper) return Boolean;

   --  Returns true if the mapper is a wildcard node (matches any element).
   function Is_Wildcard (Controller : in Mapper) return Boolean;

   --  Returns the mapping name.
   function Get_Name (Controller : in Mapper) return String;

   procedure Iterate (Controller : in Mapper;
                      Process : not null access procedure (Map : in Mapper'Class));

   --  Dump the mapping tree on the logger using the INFO log level.
   procedure Dump (Handler : in Mapper'Class;
                   Log     : in Util.Log.Loggers.Logger'Class;
                   Prefix  : in String := "");

private
   --  Find a path component representing a child mapper under <b>From</b> and
   --  identified by the given <b>Name</b>.  If the mapper is not found, a new
   --  Mapper_Node is created.
   procedure Find_Path_Component (From   : in out Mapper'Class;
                                  Name   : in String;
                                  Root   : in out Mapper_Access;
                                  Result : out Mapper_Access);

   --  Build the mapping tree that corresponds to the given <b>Path</b>.
   --  Each path component is represented by a <b>Mapper_Node</b> element.
   --  The node is created if it does not exists.
   procedure Build_Path (Into     : in out Mapper'Class;
                         Path     : in String;
                         Last_Pos : out Natural;
                         Node     : out Mapper_Access);

   type Mapping is abstract tagged limited record
      Mapper       : Mapper_Access;
      Name         : Ada.Strings.Unbounded.Unbounded_String;
      Is_Attribute : Boolean := False;
   end record;

   type Mapper is new Ada.Finalization.Limited_Controlled with record
      Next_Mapping     : Mapper_Access := null;
      First_Child      : Mapper_Access := null;
      Mapper           : Mapper_Access := null;
      Mapping          : Mapping_Access := null;
      Name             : Ada.Strings.Unbounded.Unbounded_String;
      Is_Proxy_Mapper  : Boolean := False;
      Is_Clone         : Boolean := False;
      Is_Wildcard      : Boolean := False;
      Is_Deep_Wildcard : Boolean := False;
   end record;

   --  Finalize the object and release any mapping.
   overriding
   procedure Finalize (Controller : in out Mapper);

end Util.Serialize.Mappers;
