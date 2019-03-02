-----------------------------------------------------------------------
--  util-commands-consoles-text -- Text console interface
--  Copyright (C) 2014, 2015, 2017, 2018 Stephane Carrez
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

package body Util.Commands.Consoles.Text is

   --  ------------------------------
   --  Report an error message.
   --  ------------------------------
   overriding
   procedure Error (Console : in out Console_Type;
                    Message : in String) is
      pragma Unreferenced (Console);
   begin
      Ada.Text_IO.Put_Line (Message);
   end Error;

   --  ------------------------------
   --  Report a notice message.
   --  ------------------------------
   overriding
   procedure Notice (Console : in out Console_Type;
                     Kind    : in Notice_Type;
                     Message : in String) is
      pragma Unreferenced (Console, Kind);
   begin
      Ada.Text_IO.Put_Line (Message);
   end Notice;

   --  ------------------------------
   --  Print the field value for the given field.
   --  ------------------------------
   overriding
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in String;
                          Justify : in Justify_Type := J_LEFT) is
      use type Ada.Text_IO.Count;

      Pos   : constant Ada.Text_IO.Count := Ada.Text_IO.Count (Console.Cols (Field));
      Size  : constant Natural := Console.Sizes (Field);
      Start : Natural := Value'First;
      Last  : constant Natural := Value'Last;
      Pad   : Natural := 0;
   begin
      case Justify is
         when J_LEFT =>
            if Value'Length > Size and Size > 0 then
               Start := Last - Size + 1;
            end if;

         when J_RIGHT =>
            if Value'Length < Size then
               Pad := Size - Value'Length - 1;
            else
               Start := Last - Size + 1;
            end if;

         when J_CENTER =>
            if Value'Length < Size then
               Pad  := (Size - Value'Length) / 2;
            else
               Start := Last - Size + 1;
            end if;

         when J_RIGHT_NO_FILL =>
            if Value'Length >= Size then
               Start := Last - Size + 1;
            end if;

      end case;
      if Pad > 0 then
         Ada.Text_IO.Set_Col (Pos + Ada.Text_IO.Count (Pad));
      elsif Pos > 1 then
         Ada.Text_IO.Set_Col (Pos);
      end if;
      Ada.Text_IO.Put (Value (Start .. Last));
   end Print_Field;

   --  ------------------------------
   --  Print the title for the given field.
   --  ------------------------------
   overriding
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String) is
      use type Ada.Text_IO.Count;

      Pos : constant Ada.Text_IO.Count := Ada.Text_IO.Count (Console.Cols (Field));
   begin
      if Pos > 1 then
         Ada.Text_IO.Set_Col (Pos);
      end if;
      Ada.Text_IO.Put (Title);
   end Print_Title;

   --  ------------------------------
   --  Start a new title in a report.
   --  ------------------------------
   overriding
   procedure Start_Title (Console : in out Console_Type) is
   begin
      Console.Field_Count := 0;
      Console.Sizes := (others => 0);
      Console.Cols := (others => 1);
   end Start_Title;

   --  ------------------------------
   --  Finish a new title in a report.
   --  ------------------------------
   procedure End_Title (Console : in out Console_Type) is
      pragma Unreferenced (Console);
   begin
      Ada.Text_IO.New_Line;
   end End_Title;

   --  ------------------------------
   --  Start a new row in a report.
   --  ------------------------------
   overriding
   procedure Start_Row (Console : in out Console_Type) is
      pragma Unreferenced (Console);
   begin
      null;
   end Start_Row;

   --  ------------------------------
   --  Finish a new row in a report.
   --  ------------------------------
   overriding
   procedure End_Row (Console : in out Console_Type) is
      pragma Unreferenced (Console);
   begin
      Ada.Text_IO.New_Line;
   end End_Row;

end Util.Commands.Consoles.Text;
