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

with Ada.Unchecked_Deallocation;
package body Util.Serialize.Mappers is

   procedure Free is new Ada.Unchecked_Deallocation (Mapping'Class, Mapping_Access);

   --  -----------------------
   --  Bind the name and the handler in the current mapper.
   --  -----------------------
   procedure Add_Member (Controller : in out Mapper;
                         Name       : in String;
                         Handler    : in Mapper_Access) is
   begin
      Controller.Mapping.Insert (Key      => Name,
                                 New_Item => Handler);
   end Add_Member;

   --  -----------------------
   --  Add the member to the current mapper.
   --  -----------------------
   procedure Add_Member (Controller : in out Mapper;
                         Name       : in String) is
   begin
      Controller.Mapping.Insert (Key => Name, New_Item => null);
   end Add_Member;

   --  -----------------------
   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   --  -----------------------
   function Find_Mapping (Controller : in Mapper;
                         Name       : in String) return Mapping_Access is
      Pos : constant Mapping_Map.Cursor := Controller.Rules.Find (Key => Name);
   begin
      if Mapping_Map.Has_Element (Pos) then
         return Mapping_Map.Element (Pos);
      else
         return null;
      end if;
   end Find_Mapping;

   --  Find the mapper associated with the given name.
   --  Returns null if there is no mapper.
   function Find_Mapper (Controller : in Mapper;
                         Name       : in String) return Mapper_Access is
      Pos : constant Mapper_Map.Cursor := Controller.Mapping.Find (Key => Name);
   begin
      if Mapper_Map.Has_Element (Pos) then
         return Mapper_Map.Element (Pos);
      else
         return null;
      end if;
   end Find_Mapper;

   procedure Add_Mapping (Into : in out Mapper;
                          Path : in String;
                          Map  : in Mapping_Access) is
   begin
      Into.Rules.Insert (Key => Path, New_Item => Map);
   end Add_Mapping;

   procedure Add_Mapping (Into : in out Mapper;
                          Path : in String;
                          Map  : in Mapper_Access) is
   begin
      Into.Mapping.Insert (Key => Path, New_Item => Map);
   end Add_Mapping;

   procedure Start_Object (Handler : in Mapper;
                           Context : in out Util.Serialize.Contexts.Context'Class;
                           Name    : in String) is
   begin
      null;
   end Start_Object;

   procedure Finish_Object (Handler : in Mapper;
                            Context : in out Util.Serialize.Contexts.Context'Class;
                            Name    : in String) is
   begin
      null;
   end Finish_Object;

   --  -----------------------
   --  Finalize the object and release any mapping.
   --  -----------------------
   overriding
   procedure Finalize (Controller : in out Mapper) is
      Pos     : Mapping_Map.Cursor;
      Content : Mapping_Access;
   begin
      loop
         Pos := Controller.Rules.First;
         exit when not Mapping_Map.Has_Element (Pos);
         Content := Mapping_Map.Element (Pos);
         Free (Content);
         Controller.Rules.Delete (Pos);
      end loop;
   end Finalize;

end Util.Serialize.Mappers;
