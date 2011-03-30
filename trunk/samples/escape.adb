-----------------------------------------------------------------------
--  escape -- Text Transformations
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011 Stephane Carrez
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
with Util.Strings.Transforms;
with Ada.Text_IO;
with Ada.Command_Line;
with Util.Strings;
procedure Escape is

   Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: escape string...");
      return;
   end if;
   for I in 1 .. Count loop
      declare
         S : constant String := Ada.Command_Line.Argument (I);
      begin
         Ada.Text_IO.Put_Line ("Escape javascript : "
                               & Util.Strings.Transforms.Escape_Javascript (S));
         Ada.Text_IO.Put_Line ("Escape XML        : "
                               & Util.Strings.Transforms.Escape_Xml (S));
      end;
   end loop;
end Escape;
