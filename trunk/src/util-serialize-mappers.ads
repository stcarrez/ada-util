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
with Util.Beans.Objects;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Util.Serialize.Contexts;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Finalization;
package Util.Serialize.Mappers is

   Mapping_Error : exception;

   --  ------------------------------
   --  Mapping node
   --  ------------------------------
   --  The <b>Mapping</b> represents a rule component to establish a mapping
   --  when reading some format (XML, JSON).
   type Mapping is abstract tagged limited private;
   type Mapping_Access is access all Mapping'Class;

   --  Execute the rule associated with the mapping.
   procedure Execute (Map     : in Mapping;
                      Context : in out Util.Serialize.Contexts.Context'Class;
                      Value   : in Util.Beans.Objects.Object) is abstract;

   --  ------------------------------
   --  Mapper
   --  ------------------------------
   type Mapper is abstract new Ada.Finalization.Limited_Controlled with private;
   type Mapper_Access is access all Mapper'Class;

   --  Execute the mapping operation on the object associated with the current context.
   --  The object is extracted from the context and the <b>Execute</b> operation is called.
   procedure Execute (Handler : in Mapper;
                      Map     : in Mapping'Class;
                      Ctx     : in out Util.Serialize.Contexts.Context'Class;
                      Value   : in Util.Beans.Objects.Object) is abstract;

   --  Bind the name and the handler in the current mapper.
   procedure Add_Member (Controller : in out Mapper;
                         Name       : in String;
                         Handler    : in Mapper_Access);

   --  Add the member to the current mapper.
   procedure Add_Member (Controller : in out Mapper;
                         Name       : in String);

   --  Add a mapping for setting a member.  When the attribute rule defined by <b>Path</b>
   --  is matched, the <b>Set_Member</b> procedure will be called with the value and the
   --  <b>Field</b> identification.
   procedure Add_Mapping (Into : in out Mapper;
                          Path : in String;
                          Map  : in Mapping_Access);

   procedure Add_Mapping (Into : in out Mapper;
                          Path : in String;
                          Map  : in Mapper_Access);

   --  Set the name/value pair on the current object.  For each active mapping,
   --  find whether a rule matches our name and execute it.
   procedure Set_Member (Handler : in Mapper;
                         Name    : in String;
                         Value   : in Util.Beans.Objects.Object;
                         Context : in out Util.Serialize.Contexts.Context'Class);

   procedure Start_Object (Handler : in Mapper;
                           Context : in out Util.Serialize.Contexts.Context'Class;
                           Name    : in String);

   procedure Finish_Object (Handler : in Mapper;
                            Context : in out Util.Serialize.Contexts.Context'Class;
                            Name    : in String);

   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   function Find_Mapping (Controller : in Mapper;
                          Name       : in String) return Mapping_Access;

   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   function Find_Mapper (Controller : in Mapper;
                         Name       : in String) return Mapper_Access;


   package Mapper_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Mapper_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

private

   type Mapping is abstract tagged limited record
      Mapper : Mapper_Access;
      Name   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

--     type Entity_Mapping is new Mapping with null record;
--
--     type Attribute_Mapping is new Mapping with record
--        Target : Ada.Strings.Unbounded.Unbounded_String;
--     end record;

--     type Node_Mapping is new Mapping with record
--        Next  : Mapping_Access;
--        Child : Mapping_Access;
--     end record;

   package Mapping_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Mapping_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Mapper is abstract new Ada.Finalization.Limited_Controlled with record
      Mapping : Mapper_Map.Map;
      Rules   : Mapping_Map.Map;
   end record;

   --  Finalize the object and release any mapping.
   overriding
   procedure Finalize (Controller : in out Mapper);
--
--     type Proxy_Mapper is abstract new Mapper with record
--        Mapper : Mapper_Access;
--     end record;
--     type Proxy_Mapper_Access is access all Proxy_Mapper'Class;

end Util.Serialize.Mappers;
