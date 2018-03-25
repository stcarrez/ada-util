-----------------------------------------------------------------------
--  util-commands -- Support to make command line tools
--  Copyright (C) 2017, 2018 Stephane Carrez
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
package Util.Commands is

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
   function Get_Count (List : in Dynamic_Argument_List) return Natural;

   --  Get the argument at the given position.
   function Get_Argument (List : in Dynamic_Argument_List;
                          Pos  : in Positive) return String;

   --  Get the command name.
   function Get_Command_Name (List : in Dynamic_Argument_List) return String;

private

   type Argument_Pos is array (Natural range <>) of Natural;

   type String_Argument_List (Max_Length : Positive;
                              Max_Args   : Positive) is new Argument_List
     with record
      Count     : Natural := 0;
      Length    : Natural := 0;
      Line      : String (1 .. Max_Length);
      Start_Pos : Argument_Pos (0 .. Max_Args);
      End_Pos   : Argument_Pos (0 .. Max_Args);
   end record;

   type Dynamic_Argument_List is limited new Argument_List with record
      List : Util.Strings.Vectors.Vector;
   end record;

end Util.Commands;
