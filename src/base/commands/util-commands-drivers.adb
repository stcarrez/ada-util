-----------------------------------------------------------------------
--  util-commands-drivers -- Support to make command line tools
--  Copyright (C) 2017, 2018, 2019, 2021, 2022 Stephane Carrez
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
with Util.Strings.Formats;
with Ada.Text_IO; use Ada.Text_IO;
package body Util.Commands.Drivers is

   use Ada.Strings.Unbounded;

   --  The logger
   Logs : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create (Driver_Name);

   function "-" (Message : in String) return String is (Translate (Message)) with Inline;

   --  ------------------------------
   --  Get the description associated with the command.
   --  ------------------------------
   function Get_Description (Command : in Command_Type) return String is
   begin
      return To_String (Command.Description);
   end Get_Description;

   --  ------------------------------
   --  Get the name used to register the command.
   --  ------------------------------
   function Get_Name (Command : in Command_Type) return String is
   begin
      return To_String (Command.Name);
   end Get_Name;

   --  ------------------------------
   --  Write the command usage.
   --  ------------------------------
   procedure Usage (Command : in out Command_Type;
                    Name    : in String;
                    Context : in out Context_Type) is
      Config  : Config_Type;
   begin
      Command_Type'Class (Command).Setup (Config, Context);
      Config_Parser.Usage (Name, Config);
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
   procedure Execute (Command   : in out Help_Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      procedure Compute_Size (Position : in Command_Sets.Cursor);
      procedure Print (Position : in Command_Sets.Cursor);

      Column : Ada.Text_IO.Positive_Count := 1;

      procedure Compute_Size (Position : in Command_Sets.Cursor) is
         Cmd  : constant Command_Access := Command_Sets.Element (Position);
         Len  : constant Natural := Length (Cmd.Name);
      begin
         if Natural (Column) < Len then
            Column := Ada.Text_IO.Positive_Count (Len);
         end if;
      end Compute_Size;

      procedure Print (Position : in Command_Sets.Cursor) is
         Cmd  : constant Command_Access := Command_Sets.Element (Position);
      begin
         Put ("   ");
         Put (To_String (Cmd.Name));
         if Length (Cmd.Description) > 0 then
            Set_Col (Column + 7);
            Put (To_String (Cmd.Description));
         end if;
         New_Line;
      end Print;

   begin
      Logs.Debug ("Execute command {0}", Name);

      if Args.Get_Count = 0 then
         Usage (Command.Driver.all, Args, Context);
         New_Line;
         Put_Line (Strings.Formats.Format (-("Type '{0} help {command}' for help "
                                               & "on a specific command."), Driver_Name));
         Put_Line (-("Available subcommands:"));
         Command.Driver.List.Iterate (Process => Compute_Size'Access);
         Command.Driver.List.Iterate (Process => Print'Access);
      else
         declare
            Cmd_Name   : constant String := Args.Get_Argument (1);
            Target_Cmd : constant Command_Access := Command.Driver.Find_Command (Cmd_Name);
         begin
            if Target_Cmd = null then
               Logs.Error (-("unknown command '{0}'"), Cmd_Name);
               raise Not_Found;
            else
               Target_Cmd.Help (Cmd_Name, Context);
            end if;
         end;
      end if;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in out Help_Command_Type;
                   Name      : in String;
                   Context   : in out Context_Type) is
   begin
      null;
   end Help;

   --  ------------------------------
   --  Report the command usage.
   --  ------------------------------
   procedure Usage (Driver  : in Driver_Type;
                    Args    : in Argument_List'Class;
                    Context : in out Context_Type;
                    Name    : in String := "") is
   begin
      Put_Line (To_String (Driver.Desc));
      New_Line;
      if Name'Length > 0 then
         declare
            Command : constant Command_Access := Driver.Find_Command (Name);
         begin
            if Command /= null then
               Command.Usage (Name, Context);
            else
               Put (-("Invalid command"));
            end if;
         end;
      else
         Put (-("Usage: "));
         Put (Args.Get_Command_Name);
         Put (" ");
         Put_Line (To_String (Driver.Usage));
      end if;
   end Usage;

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
      Command.Name := To_Unbounded_String (Name);
      Command.Driver := Driver'Unchecked_Access;
      Driver.List.Include (Command);
   end Add_Command;

   procedure Add_Command (Driver      : in out Driver_Type;
                          Name        : in String;
                          Description : in String;
                          Command     : in Command_Access) is
   begin
      Command.Name := To_Unbounded_String (Name);
      Command.Description := To_Unbounded_String (Description);
      Add_Command (Driver, Name, Command);
   end Add_Command;

   --  ------------------------------
   --  Register the command under the given name.
   --  ------------------------------
   procedure Add_Command (Driver      : in out Driver_Type;
                          Name        : in String;
                          Description : in String;
                          Handler     : in Command_Handler) is
      Command : constant Command_Access
        := new Handler_Command_Type '(Driver      => Driver'Unchecked_Access,
                                      Description => To_Unbounded_String (Description),
                                      Name        => To_Unbounded_String (Name),
                                      Handler     => Handler);
   begin
      Driver.List.Include (Command);
   end Add_Command;

   --  ------------------------------
   --  Find the command having the given name.
   --  Returns null if the command was not found.
   --  ------------------------------
   function Find_Command (Driver : in Driver_Type;
                          Name   : in String) return Command_Access is
      Cmd : aliased Help_Command_Type;
      Pos : Command_Sets.Cursor;
   begin
      Cmd.Name := To_Unbounded_String (Name);
      Pos := Driver.List.Find (Cmd'Unchecked_Access);
      if Command_Sets.Has_Element (Pos) then
         return Command_Sets.Element (Pos);
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
      procedure Execute (Cmd_Args : in Argument_List'Class);

      Command : constant Command_Access := Driver.Find_Command (Name);

      procedure Execute (Cmd_Args : in Argument_List'Class) is
      begin
         Command.Execute (Name, Cmd_Args, Context);
      end Execute;

   begin
      if Command /= null then
         declare
            Config  : Config_Type;
         begin
            Command.Setup (Config, Context);
            Config_Parser.Execute (Config, Args, Execute'Access);
         end;
      else
         Logs.Error (-("unknown command '{0}'"), Name);
         raise Not_Found;
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
      pragma Unreferenced (Driver);
   begin
      Logs.Print (Level, "{0}: {1}", Name, Message);
   end Log;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in out Handler_Command_Type;
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
   procedure Help (Command   : in out Handler_Command_Type;
                   Name      : in String;
                   Context   : in out Context_Type) is
   begin
      null;
   end Help;

end Util.Commands.Drivers;
