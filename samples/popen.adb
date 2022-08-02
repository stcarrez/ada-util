-----------------------------------------------------------------------
--  popen -- Print the GNAT version by using a pipe
--  Copyright (C) 2011, 2021 Stephane Carrez
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
