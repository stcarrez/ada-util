-----------------------------------------------------------------------
--  popen -- Print the GNAT version by using a pipe
--  Copyright (C) 2011, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Util.Processes;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
procedure Popen is

   Command : constant String := "gnatmake --version";

   Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
   Buffer  : Util.Streams.Buffered.Input_Buffer_Stream;
   Content : Ada.Strings.Unbounded.Unbounded_String;
begin
   Pipe.Open (Command, Util.Processes.READ);
   Buffer.Initialize (Pipe'Unchecked_Access, 1024);
   Buffer.Read (Content);
   Pipe.Close;

   if Pipe.Get_Exit_Status /= 0 then
      Ada.Text_IO.Put_Line (Command & " exited with status "
                            & Integer'Image (Pipe.Get_Exit_Status));
      return;
   end if;

   Ada.Text_IO.Put_Line ("Result: " & Ada.Strings.Unbounded.To_String (Content));
end Popen;
