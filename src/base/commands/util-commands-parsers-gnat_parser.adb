-----------------------------------------------------------------------
--  util-commands-parsers.gnat_parser -- GNAT command line parser for command drivers
--  Copyright (C) 2018, 2019, 2021 Stephane Carrez
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

   --  ------------------------------
   --  Get all the remaining arguments from the GNAT command line parse.
   --  ------------------------------
   procedure Get_Arguments (List    : in out Dynamic_Argument_List;
                            Command : in String;
                            Parser  : in GC.Opt_Parser := GC.Command_Line_Parser) is
   begin
      List.Name := Ada.Strings.Unbounded.To_Unbounded_String (Command);
      loop
         declare
            S : constant String := GC.Get_Argument (Parser => Parser);
         begin
            exit when S'Length = 0;
            List.List.Append (S);
         end;
      end loop;
   end Get_Arguments;

   procedure Execute (Config : in out Config_Type;
                      Args   : in Util.Commands.Argument_List'Class;
                      Process : not null access
                        procedure (Cmd_Args : in Commands.Argument_List'Class)) is
      use type GC.Command_Line_Configuration;

      Empty    : Config_Type;
   begin
      if Config /= Empty then
         declare
            Parser   : GC.Opt_Parser;
            Cmd_Args : Dynamic_Argument_List;
            Params   : GNAT.OS_Lib.Argument_List_Access
              := To_OS_Lib_Argument_List (Args);
         begin
            GC.Initialize_Option_Scan (Parser, Params);
            GC.Getopt (Config => Config, Parser => Parser);
            Get_Arguments (Cmd_Args, Args.Get_Command_Name, Parser);
            Process (Cmd_Args);
            GC.Free (Config);
            GNAT.OS_Lib.Free (Params);

         exception
            when others =>
               GNAT.OS_Lib.Free (Params);
               GC.Free (Config);
               raise;
         end;
      else
         Process (Args);
      end if;
   end Execute;

   procedure Usage (Name   : in String;
                    Config : in out Config_Type) is
      Opts : constant String := GC.Get_Switches (Config);
   begin
      if Opts'Length > 0 then
         GC.Set_Usage (Config, Usage => Name & " [switches] [arguments]");
         GC.Display_Help (Config);
      end if;
   end Usage;

end Util.Commands.Parsers.GNAT_Parser;
