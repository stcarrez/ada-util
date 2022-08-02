# Command Line Utilities
The `Util.Commands` package provides a support to help in writing command line
applications.  It allows to have several commands in the application, each of them
being identified by a unique name.  Each command has its own options and arguments.
The command line support is built around several children packages.

The `Util.Commands.Drivers` package is a generic package that must be instantiated
to define the list of commands that the application supports.  It provides operations
to register commands and then to execute them with a list of arguments.  When a
command is executed, it gets its name, the command arguments and an application
context.  The application context can be used to provide arbitrary information that
is needed by the application.

The `Util.Commands.Parsers` package provides the support to parse the command
line arguments.

The `Util.Commands.Consoles` package is a generic package that can help for the
implementation of a command to display its results.  Its use is optional.

## Command arguments
The `Argument_List` interface defines a common interface to get access to the command
line arguments.  It has several concrete implementations.  This is the interface type
that is used by commands registered and executed in the driver.

The `Default_Argument_List` gives access to the program command line arguments through
the `Ada.Command_Line` package.

The `String_Argument_List` allows to split a string into a list of arguments.  It can
be used to build new command line arguments.

## Command line driver
The `Util.Commands.Drivers` generic package provides a support to build command line
tools that have different commands identified by a name.  It defines the `Driver_Type`
tagged record that provides a registry of application commands.  It gives entry points
to register commands and execute them.

The `Context_Type` package parameter defines the type for the `Context` parameter
that is passed to the command when it is executed.  It can be used to provide
application specific context to the command.

The `Config_Parser` describes the parser package that will handle the analysis of
command line options.  To use the GNAT options parser, it is possible to use the
`Util.Commands.Parsers.GNAT_Parser` package.

## Command line parsers
Parsing command line arguments before their execution is handled by the
`Config_Parser` generic package.  This allows to customize how the arguments are
parsed.

The `Util.Commands.Parsers.No_Parser` package can be used to execute the command
without parsing its arguments.

The `Util.Commands.Parsers.GNAT_Parser.Config_Parser` package provides support to
parse command line arguments by using the `GNAT` `Getopt` support.

## Example
First, an application context type is defined to allow a command to get some application
specific information.  The context type is passed during the instantiation of the
`Util.Commands.Drivers` package and will be passed to commands through the `Execute`
procedure.

```Ada
type Context_Type is limited record
   ... --  Some application specific data
end record;
package Drivers is
  new Util.Commands.Drivers
    (Context_Type  => Context_Type,
     Config_Parser => Util.Commands.Parsers.GNAT_Parser.Config_Parser,
     Driver_Name   => "tool");

```

Then an instance of the command driver must be declared.  Commands are then registered
to the command driver so that it is able to find them and execute them.

```Ada
 Driver : Drivers.Driver_Type;
```

A command can be implemented by a simple procedure or by using the `Command_Type`
abstract tagged record and implementing the `Execute` procedure:

```Ada
procedure Command_1 (Name    : in String;
                     Args    : in Argument_List'Class;
                     Context : in out Context_Type);
type My_Command is new Drivers.Command_Type with null record;
procedure Execute (Command : in out My_Command;
                   Name    : in String;
                   Args    : in Argument_List'Class;
                   Context : in out Context_Type);

```

Commands are registered during the application initialization.
And registered in the driver by using the `Add_Command` procedure:

```Ada
Driver.Add_Command (Name => "cmd1",
                    Description => "",
                    Handler => Command_1'Access);
```

A command is executed by giving its name and a list of arguments.  By using the
`Default_Argument_List` type, it is possible to give to the command the application
command line arguments.

```Ada
Ctx   : Context_Type;
Args  : Util.Commands.Default_Argument_List (0);
...
Driver.Execute ("cmd1", Args, Ctx);
```

