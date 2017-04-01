-----------------------------------------------------------------------
--  util-commands-drivers -- Support to make command line tools
--  Copyright (C) 2017 Stephane Carrez
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
with Util.Log;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Ordered_Maps;

--  == Command line driver ==
--  The <tt>Util.Commands.Drivers</tt> generic package provides a support to build command line
--  tools that have different commands identified by a name.
generic
   --  The command execution context.
   type Context_Type (<>) is limited private;
   Driver_Name : String := "Drivers";
package Util.Commands.Drivers is

   --  A simple command handler executed when the command with the given name is executed.
   type Command_Handler is not null access procedure (Name    : in String;
                                                      Args    : in Argument_List'Class;
                                                      Context : in out Context_Type);

   --  A more complex command handler that has a command instance as context.
   type Command_Type is abstract tagged limited private;
   type Command_Access is access all Command_Type'Class;

   --  Execute the command with the arguments.  The command name is passed with the command
   --  arguments.
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is abstract;

   --  Write the help associated with the command.
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is abstract;

   --  Write the command usage.
   procedure Usage (Command : in Command_Type);

   --  Print a message for the command.  The level indicates whether the message is an error,
   --  warning or informational.  The command name can be used to known the originator.
   --  The <tt>Log</tt> operation is redirected to the driver's <tt>Log</tt> procedure.
   procedure Log (Command : in Command_Type;
                  Level   : in Util.Log.Level_Type;
                  Name    : in String;
                  Message : in String);

   type Help_Command_Type is new Command_Type with private;

   --  Execute the help command with the arguments.
   --  Print the help for every registered command.
   overriding
   procedure Execute (Command   : in Help_Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type);

   --  Write the help associated with the command.
   procedure Help (Command   : in Help_Command_Type;
                   Context   : in out Context_Type);

   type Driver_Type is tagged limited private;

   --  Set the driver description printed in the usage.
   procedure Set_Description (Driver      : in out Driver_Type;
                              Description : in String);

   --  Set the driver usage printed in the usage.
   procedure Set_Usage (Driver  : in out Driver_Type;
                        Usage   : in String);

   --  Register the command under the given name.
   procedure Add_Command (Driver  : in out Driver_Type;
                          Name    : in String;
                          Command : in Command_Access);

   --  Register the command under the given name.
   procedure Add_Command (Driver  : in out Driver_Type;
                          Name    : in String;
                          Handler : in Command_Handler);

   --  Find the command having the given name.
   --  Returns null if the command was not found.
   function Find_Command (Driver : in Driver_Type;
                          Name   : in String) return Command_Access;

   --  Execute the command registered under the given name.
   procedure Execute (Driver  : in Driver_Type;
                      Name    : in String;
                      Args    : in Argument_List'Class;
                      Context : in out Context_Type);

   --  Print a message for the command.  The level indicates whether the message is an error,
   --  warning or informational.  The command name can be used to known the originator.
   procedure Log (Driver  : in Driver_Type;
                  Level   : in Util.Log.Level_Type;
                  Name    : in String;
                  Message : in String);

private

   package Command_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => Command_Access,
                                                 "<"          => "<");

   type Command_Type is abstract tagged limited record
      Driver : access Driver_Type'Class;
   end record;

   type Help_Command_Type is new Command_Type with null record;

   type Handler_Command_Type is new Command_Type with record
      Handler : Command_Handler;
   end record;

   --  Execute the command with the arguments.
   overriding
   procedure Execute (Command   : in Handler_Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type);

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in Handler_Command_Type;
                   Context   : in out Context_Type);

   type Driver_Type is tagged limited record
      List  : Command_Maps.Map;
      Desc  : Ada.Strings.Unbounded.Unbounded_String;
      Usage : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Util.Commands.Drivers;
