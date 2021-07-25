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
with GNAT.Command_Line;

package Util.Commands.Parsers.GNAT_Parser is

   package GC renames GNAT.Command_Line;

   subtype Config_Type is GC.Command_Line_Configuration;

   procedure Execute (Config  : in out Config_Type;
                      Args    : in Util.Commands.Argument_List'Class;
                      Process : not null access
                        procedure (Cmd_Args : in Commands.Argument_List'Class));

   procedure Usage (Name   : in String;
                    Config : in out Config_Type);

   --  Get all the remaining arguments from the GNAT command line parse.
   procedure Get_Arguments (List    : in out Dynamic_Argument_List;
                            Command : in String;
                            Parser  : in GC.Opt_Parser := GC.Command_Line_Parser);

   package Config_Parser is
     new Util.Commands.Parsers.Config_Parser (Config_Type => Config_Type,
                                              Execute     => Execute);

end Util.Commands.Parsers.GNAT_Parser;
