-----------------------------------------------------------------------
--  launch -- Launch an external process redirecting the input and output
--  Copyright (C) 2011 Stephane Carrez
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

with Util.Processes;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Streams.Texts;
procedure Launch is
   use Ada.Strings.Unbounded;

   Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
   Buffer  : Util.Streams.Buffered.Input_Buffer_Stream;
   Content : Unbounded_String;
   Print   : Util.Streams.Texts.Print_Stream;
begin
      --  Write on the process input stream
   Pipe.Open ("nslookup", Util.Processes.READ_WRITE);
   Buffer.Initialize (Pipe'Unchecked_Access, 1024);
   Print.Initialize (Pipe'Unchecked_Access);

   --  Write on the 'nslookup' input pipe a list of domains to resolve.
   Print.Write ("www.google.com" & ASCII.LF);
   Print.Write ("set type=NS" & ASCII.LF);
   Print.Write ("www.google.com" & ASCII.LF);
   Print.Write ("set type=MX" & ASCII.LF);
   Print.Write ("www.google.com" & ASCII.LF);
   Print.Close;

   --  Read the 'nslookup' output.
   Buffer.Read (Content);
   Pipe.Close;
   Ada.Text_IO.Put_Line ("Result lenght: " & Integer'Image (Length (Content)));
   Ada.Text_IO.Put_Line ("Exit status: " & Integer'Image (Pipe.Get_Exit_Status));
   Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Content));
end Launch;
