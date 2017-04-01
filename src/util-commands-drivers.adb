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
with Util.Log.Loggers;
with Ada.Text_IO; use Ada.Text_IO;
package body Util.Commands.Drivers is

   --  The logger
   Logs : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create (Driver_Name);

   --  ------------------------------
   --  Write the command usage.
   --  ------------------------------
   procedure Usage (Command : in Command_Type) is
   begin
      null;
   end Usage;

   --  ------------------------------
   --  Print a message for the command.  The level indicates whether the message is an error,
   --  warning or informational.  The command name can be used to known the originator.
   --  The <tt>Log</tt> operation is redirected to the driver's <tt>Log</tt> procedure.
   --  ------------------------------
   procedure Log (Command : in Command_Type;
                  Level   : in Util.Log.Level_Type;
                  Name    : in String;
                  Message : in String) is
   begin
      Command.Driver.Log (Level, Name, Message);
   end Log;

   --  ------------------------------
   --  Execute the help command with the arguments.
   --  Print the help for every registered command.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Help_Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
     procedure Print (Position : in Command_Maps.Cursor);

      procedure Print (Position : in Command_Maps.Cursor) is
         Name : constant String := Command_Maps.Key (Position);
      begin
         Put_Line ("   " & Name);
      end Print;

   begin
      Logs.Debug ("Execute command {0}", Name);

      if Args.Get_Count = 0 then
         --  Usage;
         New_Line;
         Put ("Type '");
         Put (Args.Get_Command_Name);
         Put_Line (" help {command}' for help on a specific command.");
         New_Line;
         Put_Line ("Available subcommands:");

         Command.Driver.List.Iterate (Process => Print'Access);
      else
         declare
            Cmd_Name   : constant String := Args.Get_Argument (1);
            Target_Cmd : constant Command_Access := Command.Driver.Find_Command (Cmd_Name);
         begin
            if Target_Cmd = null then
               Logs.Error ("Unknown command {0}", Cmd_Name);
            else
               Target_Cmd.Help (Context);
            end if;
         end;
      end if;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Command   : in Help_Command_Type;
                   Context   : in out Context_Type) is
   begin
      null;
   end Help;

   --  ------------------------------
   --  Set the driver description printed in the usage.
   --  ------------------------------
   procedure Set_Description (Driver      : in out Driver_Type;
                              Description : in String) is
   begin
      Driver.Desc := Ada.Strings.Unbounded.To_Unbounded_String (Description);
   end Set_Description;

   --  ------------------------------
   --  Set the driver usage printed in the usage.
   --  ------------------------------
   procedure Set_Usage (Driver  : in out Driver_Type;
                        Usage   : in String) is
   begin
      Driver.Usage := Ada.Strings.Unbounded.To_Unbounded_String (Usage);
   end Set_Usage;

   --  ------------------------------
    --  Register the command under the given name.
   --  ------------------------------
   procedure Add_Command (Driver  : in out Driver_Type;
                          Name    : in String;
                          Command : in Command_Access) is
   begin
      Command.Driver := Driver'Unchecked_Access;
      Driver.List.Include (Name, Command);
   end Add_Command;

   --  ------------------------------
   --  Register the command under the given name.
   --  ------------------------------
   procedure Add_Command (Driver  : in out Driver_Type;
                          Name    : in String;
                          Handler : in Command_Handler) is
   begin
      Driver.List.Include (Name, new Handler_Command_Type '(Driver  => Driver'Unchecked_Access,
                                                            Handler => Handler));
   end Add_Command;

   --  ------------------------------
   --  Find the command having the given name.
   --  Returns null if the command was not found.
   --  ------------------------------
   function Find_Command (Driver : in Driver_Type;
                          Name   : in String) return Command_Access is
      Pos : constant Command_Maps.Cursor := Driver.List.Find (Name);
   begin
      if Command_Maps.Has_Element (Pos) then
         return Command_Maps.Element (Pos);
      else
         return null;
      end if;
   end Find_Command;

   --  ------------------------------
   --  Execute the command registered under the given name.
   --  ------------------------------
   procedure Execute (Driver  : in Driver_Type;
                      Name    : in String;
                      Args    : in Argument_List'Class;
                      Context : in out Context_Type) is
      Command : constant Command_Access := Driver.Find_Command (Name);
   begin
      if Command /= null then
         Command.Execute (Name, Args, Context);
      else
         Logs.Error ("Unkown command {0}", Name);
      end if;
   end Execute;

   --  ------------------------------
   --  Print a message for the command.  The level indicates whether the message is an error,
   --  warning or informational.  The command name can be used to known the originator.
   --  ------------------------------
   procedure Log (Driver  : in Driver_Type;
                  Level   : in Util.Log.Level_Type;
                  Name    : in String;
                  Message : in String) is
   begin
      Logs.Print (Level, "{0}: {1}", Name, Message);
   end Log;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Handler_Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
   begin
      Command.Handler (Name, Args, Context);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in Handler_Command_Type;
                   Context   : in out Context_Type) is
   begin
      null;
   end Help;

end Util.Commands.Drivers;
