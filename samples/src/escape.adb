-----------------------------------------------------------------------
--  escape -- Text Transformations
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
