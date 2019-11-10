-----------------------------------------------------------------------
--  util-processes-tools -- System specific and low level operations
--  Copyright (C) 2018, 2019 Stephane Carrez
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
