-----------------------------------------------------------------------
--  util-commands-parsers -- Support to parse command line options
--  Copyright (C) 2018 Stephane Carrez
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

package body Util.Commands.Parsers is

   --  ------------------------------
   --  Execute the command with its arguments (no parsing).
   --  ------------------------------
   procedure Execute (Config : in out No_Config_Type;
                      Args   : in Argument_List'Class;
                      Process : not null access
                        procedure (Cmd_Args : in Argument_List'Class)) is
      pragma Unreferenced (Config);
   begin
      Process (Args);
   end Execute;

end Util.Commands.Parsers;
