-----------------------------------------------------------------------
--  util-commands -- Support to make command line tools
--  Copyright (C) 2017 Stephane Carrez
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
with Ada.Command_Line;
package body Util.Commands is

   --  ------------------------------
   --  Get the number of arguments available.
   --  ------------------------------
   overriding
   function Get_Count (List : in Default_Argument_List) return Natural is
      Count : constant Natural := Ada.Command_Line.Argument_Count;
   begin
      if Count > List.Offset then
         return Count - List.Offset;
      else
         return 0;
      end if;
   end Get_Count;

   --  ------------------------------
   --  Get the argument at the given position.
   --  ------------------------------
   overriding
   function Get_Argument (List : in Default_Argument_List;
                          Pos  : in Positive) return String is
   begin
      return Ada.Command_Line.Argument (Pos + List.Offset);
   end Get_Argument;

   --  ------------------------------
   --  Get the command name.
   --  ------------------------------
   function Get_Command_Name (List : in Default_Argument_List) return String is
   begin
      return Ada.Command_Line.Command_Name;
   end Get_Command_Name;

end Util.Commands;
