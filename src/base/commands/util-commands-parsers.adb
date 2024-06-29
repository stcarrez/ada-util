-----------------------------------------------------------------------
--  util-commands-parsers -- Support to parse command line options
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
