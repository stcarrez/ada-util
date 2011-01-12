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

package body Util.Serialize.Mappers is

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

end Util.Serialize.Mappers;
