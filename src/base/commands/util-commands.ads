-----------------------------------------------------------------------
--  util-commands -- Support to make command line tools
--  Copyright (C) 2017, 2018, 2019, 2022 Stephane Carrez
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
private with Util.Strings.Vectors;
private with Ada.Strings.Unbounded;

--  = Command Line Utilities =
--  The `Util.Commands` package provides a support to help in writing command line
--  applications.  It allows to have several commands in the application, each of them
--  being identified by a unique name.  Each command has its own options and arguments.
--  The command line support is built around several children packages.
--
--  The `Util.Commands.Drivers` package is a generic package that must be instantiated
--  to define the list of commands that the application supports.  It provides operations
--  to register commands and then to execute them with a list of arguments.  When a
--  command is executed, it gets its name, the command arguments and an application
--  context.  The application context can be used to provide arbitrary information that
--  is needed by the application.
--
--  The `Util.Commands.Parsers` package provides the support to parse the command
--  line arguments.
--
--  The `Util.Commands.Consoles` package is a generic package that can help for the
--  implementation of a command to display its results.  Its use is optional.
--
--  == Command arguments ==
--  The `Argument_List` interface defines a common interface to get access to the command
--  line arguments.  It has several concrete implementations.  This is the interface type
--  that is used by commands registered and executed in the driver.
--
--  The `Default_Argument_List` gives access to the program command line arguments through
--  the `Ada.Command_Line` package.
--
--  The `String_Argument_List` allows to split a string into a list of arguments.  It can
--  be used to build new command line arguments.
--
--  @include util-commands-drivers.ads
--  @include util-commands-parsers.ads
--
--  == Example ==
--  First, an application context type is defined to allow a command to get some application
--  specific information.  The context type is passed during the instantiation of the
--  `Util.Commands.Drivers` package and will be passed to commands through the `Execute`
--  procedure.
--
--    type Context_Type is limited record
--       ... --  Some application specific data
--    end record;
--
--    package Drivers is
--      new Util.Commands.Drivers
--        (Context_Type  => Context_Type,
--         Config_Parser => Util.Commands.Parsers.GNAT_Parser.Config_Parser,
--         Driver_Name   => "tool");
--
--  Then an instance of the command driver must be declared.  Commands are then registered
--  to the command driver so that it is able to find them and execute them.
--
--     Driver : Drivers.Driver_Type;
--
--  A command can be implemented by a simple procedure or by using the `Command_Type`
--  abstract tagged record and implementing the `Execute` procedure:
--
--    procedure Command_1 (Name    : in String;
--                         Args    : in Argument_List'Class;
--                         Context : in out Context_Type);
--
--    type My_Command is new Drivers.Command_Type with null record;
--    procedure Execute (Command : in out My_Command;
--                       Name    : in String;
--                       Args    : in Argument_List'Class;
--                       Context : in out Context_Type);
--
--  Commands are registered during the application initialization.
--  And registered in the driver by using the `Add_Command` procedure:
--
--    Driver.Add_Command (Name => "cmd1",
--                        Description => "",
--                        Handler => Command_1'Access);
--
--  A command is executed by giving its name and a list of arguments.  By using the
--  `Default_Argument_List` type, it is possible to give to the command the application
--  command line arguments.
--
--    Ctx   : Context_Type;
--    Args  : Util.Commands.Default_Argument_List (0);
--    ...
--    Driver.Execute ("cmd1", Args, Ctx);
--
package Util.Commands is

   --  Exception raised when a command was not found.
   Not_Found : exception;

   --  The argument list interface that gives access to command arguments.
   type Argument_List is limited interface;

   --  Get the number of arguments available.
   function Get_Count (List : in Argument_List) return Natural is abstract;

   --  Get the argument at the given position.
   function Get_Argument (List : in Argument_List;
                          Pos  : in Positive) return String is abstract;

   --  Get the command name.
   function Get_Command_Name (List : in Argument_List) return String is abstract;

   type Default_Argument_List (Offset : Natural) is new Argument_List with null record;

   --  Get the number of arguments available.
   overriding
   function Get_Count (List : in Default_Argument_List) return Natural;

   --  Get the argument at the given position.
   overriding
   function Get_Argument (List : in Default_Argument_List;
                          Pos  : in Positive) return String;

   --  Get the command name.
   overriding
   function Get_Command_Name (List : in Default_Argument_List) return String;

   type String_Argument_List (Max_Length : Positive;
                              Max_Args   : Positive) is new Argument_List with private;

   --  Set the argument list to the given string and split the arguments.
   procedure Initialize (List : in out String_Argument_List;
                         Line : in String);

   --  Get the number of arguments available.
   overriding
   function Get_Count (List : in String_Argument_List) return Natural;

   --  Get the argument at the given position.
   overriding
   function Get_Argument (List : in String_Argument_List;
                          Pos  : in Positive) return String;

   --  Get the command name.
   overriding
   function Get_Command_Name (List : in String_Argument_List) return String;

   --  The argument list interface that gives access to command arguments.
   type Dynamic_Argument_List is limited new Argument_List with private;

   --  Get the number of arguments available.
   overriding
   function Get_Count (List : in Dynamic_Argument_List) return Natural;

   --  Get the argument at the given position.
   overriding
   function Get_Argument (List : in Dynamic_Argument_List;
                          Pos  : in Positive) return String;

   --  Get the command name.
   overriding
   function Get_Command_Name (List : in Dynamic_Argument_List) return String;

   function No_Translate (Message : in String) return String is (Message) with Inline;

private

   type Argument_Pos is array (Natural range <>) of Natural;

   type String_Argument_List (Max_Length : Positive;
                              Max_Args   : Positive) is new Argument_List
     with record
      Count     : Natural := 0;
      Length    : Natural := 0;
      Line      : String (1 .. Max_Length);
      Start_Pos : Argument_Pos (0 .. Max_Args) := (others => 0);
      End_Pos   : Argument_Pos (0 .. Max_Args) := (others => 0);
   end record;

   type Dynamic_Argument_List is limited new Argument_List with record
      List : Util.Strings.Vectors.Vector;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Util.Commands;
