with Util.Processes;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Util.Streams.Pipes;
with Util.Streams.Buffered;
procedure Launch is
   P       : Util.Processes.Process;
   C       : Integer;
   Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
   Buffer  : Util.Streams.Buffered.Buffered_Stream;
   Content : Ada.Strings.Unbounded.Unbounded_String;
begin
   Pipe.Open ("find ..", Util.Processes.READ);
   Buffer.Initialize (null, Pipe'Unchecked_Access, 1024);
   Buffer.Read (Content);
   Pipe.Close;
   Ada.Text_IO.Put_Line ("Result lenght: " & Integer'Image (Ada.Strings.Unbounded.Length (Content)));
   Ada.Text_IO.Put_Line ("Exit status: " & Integer'Image (Pipe.Get_Exit_Value));


   Util.Processes.Spawn (P, "sh -c ls -la -a -l -L -a -i -p");
   Util.Processes.Wait (P);
   Ada.Text_IO.Put_Line ("Exit status: " & Integer'Image (C));
end Launch;
