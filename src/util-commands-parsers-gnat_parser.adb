-----------------------------------------------------------------------
--  util-commands-parsers.gnat_parser -- GNAT command line parser for command drivers
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
with GNAT.OS_Lib;

package body Util.Commands.Parsers.GNAT_Parser is

   function To_OS_Lib_Argument_List (Args : in Argument_List'Class)
                                     return GNAT.OS_Lib.Argument_List_Access;

   function To_OS_Lib_Argument_List (Args : in Argument_List'Class)
                                     return GNAT.OS_Lib.Argument_List_Access is
      Count   : constant Natural := Args.Get_Count;
      New_Arg : GNAT.OS_Lib.Argument_List (1 .. Count);
   begin
      for I in 1 .. Count loop
         New_Arg (I) := new String '(Args.Get_Argument (I));
      end loop;
      return new GNAT.OS_Lib.Argument_List '(New_Arg);
   end To_OS_Lib_Argument_List;

   procedure Execute (Config : in out Config_Type;
                      Args   : in Util.Commands.Argument_List'Class;
                      Process : access procedure (Cmd_Args : in Commands.Argument_List'Class)) is
      use type GNAT.Command_Line.Command_Line_Configuration;

      Empty    : Config_Type;
    begin
      if Config /= Empty then
         declare
            Parser   : GNAT.Command_Line.Opt_Parser;
            Cmd_Args : Dynamic_Argument_List;
            Params   : GNAT.OS_Lib.Argument_List_Access
              := To_OS_Lib_Argument_List (Args);
         begin
            GNAT.Command_Line.Initialize_Option_Scan (Parser, Params);
            GNAT.Command_Line.Getopt (Config => Config, Parser => Parser);
            loop
               declare
                  S : constant String := GNAT.Command_Line.Get_Argument (Parser => Parser);
               begin
                  exit when S'Length = 0;
                  Cmd_Args.List.Append (S);
               end;
            end loop;
            Process (Cmd_Args);
            GNAT.OS_Lib.Free (Params);

         exception
            when others =>
               GNAT.OS_Lib.Free (Params);
               raise;
         end;
      else
         Process (Args);
      end if;
   end Execute;

end Util.Commands.Parsers.GNAT_Parser;
