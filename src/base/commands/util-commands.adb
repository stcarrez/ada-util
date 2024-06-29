-----------------------------------------------------------------------
--  util-commands -- Support to make command line tools
--  Copyright (C) 2017, 2018, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Characters.Handling;
with Util.Systems.IO;
with Util.Systems.Os;
package body Util.Commands is

   --  ------------------------------
   --  Get the number of arguments available.
   --  ------------------------------
   overriding
   function Get_Count (List : in Default_Argument_List) return Natural is
      Count : constant Natural := Ada.Command_Line.Argument_Count;
   begin
      if Count > List.Offset then
         return Count - List.Offset;
      else
         return 0;
      end if;
   end Get_Count;

   --  ------------------------------
   --  Get the argument at the given position.
   --  ------------------------------
   overriding
   function Get_Argument (List : in Default_Argument_List;
                          Pos  : in Positive) return String is
   begin
      return Ada.Command_Line.Argument (Pos + List.Offset);
   end Get_Argument;

   --  ------------------------------
   --  Get the command name.
   --  ------------------------------
   overriding
   function Get_Command_Name (List : in Default_Argument_List) return String is
      pragma Unreferenced (List);
   begin
      return Ada.Command_Line.Command_Name;
   end Get_Command_Name;

   --  ------------------------------
   --  Set the argument list to the given string and split the arguments.
   --  ------------------------------
   procedure Initialize (List : in out String_Argument_List;
                         Line : in String) is
      First : Natural := Line'First;
   begin
      List.Length := Line'Length;
      List.Line (1 .. Line'Length) := Line;
      List.Count := 0;
      loop
         while First <= Line'Length
           and then Ada.Characters.Handling.Is_Space (List.Line (First)) loop
            First := First + 1;
         end loop;
         exit when First > Line'Length;
         List.Start_Pos (List.Count) := First;
         while First <= Line'Length
           and then not Ada.Characters.Handling.Is_Space (List.Line (First)) loop
            First := First + 1;
         end loop;
         List.End_Pos (List.Count) := First - 1;
         List.Count := List.Count + 1;
      end loop;
      if List.Count > 0 then
         List.Count := List.Count - 1;
      end if;
   end Initialize;

   --  ------------------------------
   --  Get the number of arguments available.
   --  ------------------------------
   overriding
   function Get_Count (List : in String_Argument_List) return Natural is
   begin
      return List.Count;
   end Get_Count;

   --  ------------------------------
   --  Get the argument at the given position.
   --  ------------------------------
   overriding
   function Get_Argument (List : in String_Argument_List;
                          Pos  : in Positive) return String is
   begin
      return List.Line (List.Start_Pos (Pos) .. List.End_Pos (Pos));
   end Get_Argument;

   --  ------------------------------
   --  Get the command name.
   --  ------------------------------
   overriding
   function Get_Command_Name (List : in String_Argument_List) return String is
   begin
      return List.Line (List.Start_Pos (0) .. List.End_Pos (0));
   end Get_Command_Name;

   --  ------------------------------
   --  Get the number of arguments available.
   --  ------------------------------
   overriding
   function Get_Count (List : in Dynamic_Argument_List) return Natural is
   begin
      return Natural (List.List.Length);
   end Get_Count;

   --  ------------------------------
   --  Get the argument at the given position.
   --  ------------------------------
   overriding
   function Get_Argument (List : in Dynamic_Argument_List;
                          Pos  : in Positive) return String is
   begin
      return List.List.Element (Pos);
   end Get_Argument;

   --  ------------------------------
   --  Get the command name.
   --  ------------------------------
   overriding
   function Get_Command_Name (List : in Dynamic_Argument_List) return String is
   begin
      return Ada.Strings.Unbounded.To_String (List.Name);
   end Get_Command_Name;

   procedure Put_Raw (Content : in String) is
   begin
      Util.Systems.IO.Put_Raw (Util.Systems.IO.STDOUT_FILENO, Content);
   end Put_Raw;

   procedure Put_Raw_Line (Content : in String) is
   begin
      Util.Systems.IO.Put_Raw (Util.Systems.IO.STDOUT_FILENO,
                               Content & Util.Systems.Os.Line_Separator);
   end Put_Raw_Line;

   procedure New_Line_Raw (Count : in Positive) is
   begin
      for I in 1 .. Count loop
         Util.Systems.IO.Put_Raw (Util.Systems.IO.STDOUT_FILENO,
                                  Util.Systems.Os.Line_Separator);
      end loop;
   end New_Line_Raw;

end Util.Commands;
