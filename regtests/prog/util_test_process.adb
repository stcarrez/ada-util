-----------------------------------------------------------------------
--  Util -- Program used for testing pipes
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Command_Line;
procedure Util_Test_Process is
   use Ada.Command_Line;

   Count : constant Natural := Ada.Command_Line.Argument_Count;
   C     : Character;
begin
   if Count < 3 then
      Ada.Text_IO.Put_Line ("Usage: test <exit-code> <mode> <data>");
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;
   Ada.Command_Line.Set_Exit_Status (Exit_Status'Value (Argument (1)));

   --  Read the standard input and write it on the output.
   if Argument (2) = "read" then
      while not Ada.Text_IO.End_Of_File loop
         Ada.Text_IO.Get (C);
         Ada.Text_IO.Put (C);
      end loop;
   end if;

   --  Write the command arguments on the output.
   for I in 3 .. Count loop
      declare
         Arg : constant String := Ada.Command_Line.Argument (I);
      begin
         Ada.Text_IO.Put_Line (Arg);
      end;
   end loop;

exception
   when Constraint_Error =>
      Ada.Text_IO.Put_Line ("Invalid exit status");
      Ada.Command_Line.Set_Exit_Status (3);
end Util_Test_Process;
