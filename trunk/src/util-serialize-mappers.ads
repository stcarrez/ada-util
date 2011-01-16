-----------------------------------------------------------------------
--  util-serialize-mappers -- Serialize objects in various formats
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
with Util.Beans.Objects;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Util.Serialize.Streamers;
with Util.Serialize.Contexts;
with Ada.Containers.Indefinite_Hashed_Maps;
package Util.Serialize.Mappers is

   --  ------------------------------
   --  Mapping node
   --  ------------------------------
   --  The <b>Mapping</b> represents a rule component to establish a mapping
   --  when reading some format (XML, JSON).
   type Mapping is abstract tagged limited private;
   type Mapping_Access is access all Mapping'Class;

   --  Execute the rule associated
   procedure Execute (Map     : in Mapping;
                      Context : in out Util.Serialize.Contexts.Context'Class;
                      Value   : in Util.Beans.Objects.Object) is abstract;

   --  ------------------------------
   --  Mapper
   --  ------------------------------
   type Mapper is abstract tagged limited private;
   type Mapper_Access is access all Mapper'Class;

   --  Start a new object associated with the given name.  This is called when
   --  the '{' is reached.  The reader must be updated so that the next
   --  <b>Set_Member</b> procedure will associate the name/value pair on the
   --  new object.
--     procedure Create_Object (Controller : in out Mapper;
--                              Name       : in String) is abstract;

   --  Finish an object associated with the given name.  The reader must be
   --  updated to be associated with the previous object.
--     procedure Finish_Object (Controller : in out Mapper;
--                              Name       : in String) is abstract;

   --  Set the name/value pair on the current object.
--     procedure Set_Member (Controller  : in out Mapper;
--                           Name        : in String;
--                           Value       : in Util.Beans.Objects.Object) is abstract;

   --  Bind the name and the handler in the current mapper.
   procedure Add_Member (Controller : in out Mapper;
                         Name       : in String;
                         Handler    : in Mapper_Access);

   --  Add the member to the current mapper.
   procedure Add_Member (Controller : in out Mapper;
                         Name       : in String);

   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   function Find_Mapper (Controller : in Mapper;
                         Name       : in String) return Mapper_Access;
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

   package Mapper_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Mapper_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Mapper is abstract tagged limited record
      Mapping : Mapper_Map.Map;
   end record;

end Util.Serialize.Mappers;
