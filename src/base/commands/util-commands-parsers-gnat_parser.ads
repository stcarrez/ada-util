-----------------------------------------------------------------------
--  util-commands-parsers.gnat_parser -- GNAT command line parser for command drivers
--  Copyright (C) 2018, 2019, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
