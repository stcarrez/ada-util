-----------------------------------------------------------------------
--  util-processes-tools -- System specific and low level operations
--  Copyright (C) 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Systems.Os;
with Util.Streams.Texts;
package body Util.Processes.Tools is

   procedure Execute (Command : in String;
                      Process : in out Util.Streams.Pipes.Pipe_Stream;
                      Output  : in out Util.Strings.Vectors.Vector;
                      Status  : out Integer) is
      Text : Util.Streams.Texts.Reader_Stream;
   begin
      Text.Initialize (Process'Unchecked_Access);
      Process.Open (Command, Util.Processes.READ);
      while not Text.Is_Eof loop
         declare
            Line : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Text.Read_Line (Line, Strip => True);
            if Ada.Strings.Unbounded.Length (Line) > 0 then
               Output.Append (Ada.Strings.Unbounded.To_String (Line));
            end if;
         end;
      end loop;
      Process.Close;
      Status := Process.Get_Exit_Status;
   end Execute;

   --  ------------------------------
   --  Execute the command and append the output in the vector array.
   --  The program output is read line by line and the standard input is closed.
   --  Return the program exit status.
   --  ------------------------------
   procedure Execute (Command : in String;
                      Output  : in out Util.Strings.Vectors.Vector;
                      Status  : out Integer) is
      Proc : Util.Streams.Pipes.Pipe_Stream;
   begin
      Proc.Add_Close (Util.Systems.Os.STDIN_FILENO);
      Execute (Command, Proc, Output, Status);
   end Execute;

   procedure Execute (Command    : in String;
                      Input_Path : in String;
                      Output     : in out Util.Strings.Vectors.Vector;
                      Status     : out Integer) is
      Proc : Util.Streams.Pipes.Pipe_Stream;
   begin
      Proc.Set_Input_Stream (Input_Path);
      Execute (Command, Proc, Output, Status);
   end Execute;

end Util.Processes.Tools;
