-----------------------------------------------------------------------
--  util-commands-consoles -- Console interface
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

with Util.Strings;
package body Util.Commands.Consoles is

   --  ------------------------------
   --  Print the title for the given field and setup the associated field size.
   --  ------------------------------
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String;
                          Length  : in Positive) is
   begin
      Console.Sizes (Field) := Length;
      if Console.Field_Count >= 1 then
         Console.Cols (Field) := Console.Cols (Console.Fields (Console.Field_Count))
           + Console.Sizes (Console.Fields (Console.Field_Count));
      else
         Console.Cols (Field) := 1;
      end if;
      Console.Field_Count := Console.Field_Count + 1;
      Console.Fields (Console.Field_Count) := Field;
      Console_Type'Class (Console).Print_Title (Field, Title);
   end Print_Title;

   --  ------------------------------
   --  Format the integer and print it for the given field.
   --  ------------------------------
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Integer;
                          Justify : in Justify_Type := J_LEFT) is
      Val : constant String := Util.Strings.Image (Value);
   begin
      Console_Type'Class (Console).Print_Field (Field, Val, Justify);
   end Print_Field;

   --  ------------------------------
   --  Format the integer and print it for the given field.
   --  ------------------------------
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Ada.Strings.Unbounded.Unbounded_String;
                          Justify : in Justify_Type := J_LEFT) is
      Item : String := Ada.Strings.Unbounded.To_String (Value);
      Size : constant Natural := Console.Sizes (Field);
   begin
      if Size <= Item'Length then
         Item (Item'Last - Size + 2 .. Item'Last - Size + 4) := "...";
         Console_Type'Class (Console).Print_Field (Field,
                                                   Item (Item'Last - Size + 2 .. Item'Last));
      else
         Console_Type'Class (Console).Print_Field (Field, Item, Justify);
      end if;
   end Print_Field;

   --  ------------------------------
   --  Get the field count that was setup through the Print_Title calls.
   --  ------------------------------
   function Get_Field_Count (Console : in Console_Type) return Natural is
   begin
      return Console.Field_Count;
   end Get_Field_Count;

   --  ------------------------------
   --  Reset the field count.
   --  ------------------------------
   procedure Clear_Fields (Console : in out Console_Type) is
   begin
      Console.Field_Count := 0;
   end Clear_Fields;

end Util.Commands.Consoles;
