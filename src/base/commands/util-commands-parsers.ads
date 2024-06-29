-----------------------------------------------------------------------
--  util-commands-parsers -- Support to parse command line options
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  == Command line parsers ==
--  Parsing command line arguments before their execution is handled by the
--  `Config_Parser` generic package.  This allows to customize how the arguments are
--  parsed.
--
--  The `Util.Commands.Parsers.No_Parser` package can be used to execute the command
--  without parsing its arguments.
--
--  The `Util.Commands.Parsers.GNAT_Parser.Config_Parser` package provides support to
--  parse command line arguments by using the `GNAT` `Getopt` support.
package Util.Commands.Parsers is

   --  The config parser that must be instantiated to provide the configuration type
   --  and the Execute procedure that will parse the arguments before executing the command.
   generic
      type Config_Type is limited private;
      with procedure Execute (Config  : in out Config_Type;
                              Args    : in Argument_List'Class;
                              Process : not null access
                                procedure (Cmd_Args : in Argument_List'Class)) is <>;
      with procedure Usage (Name   : in String;
                            Config : in out Config_Type) is <>;
   package Config_Parser is
   end Config_Parser;

   --  The empty parser.
   type No_Config_Type is limited null record;

   --  Execute the command with its arguments (no parsing).
   procedure Execute (Config : in out No_Config_Type;
                      Args   : in Argument_List'Class;
                      Process : not null access
                                procedure (Cmd_Args : in Argument_List'Class));

   procedure Usage (Name   : in String;
                    Config : in out No_Config_Type) is null;

   --  A parser that executes the command immediately (no parsing of arguments).
   package No_Parser is
     new Config_Parser (Config_Type => No_Config_Type,
                        Execute     => Execute,
                        Usage       => Usage);

end Util.Commands.Parsers;
