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
with Ada.Strings.Unbounded;
generic
   type Field_Type is (<>);
   type Notice_Type is (<>);
package Util.Commands.Consoles is

   type Justify_Type is (J_LEFT,           --  Justify left   |item    |
                         J_RIGHT,          --  Justify right  |    item|
                         J_CENTER,         --  Justify center |  item  |
                         J_RIGHT_NO_FILL   --  Justify right  |item|
                        );

   type Console_Type is abstract tagged limited private;
   type Console_Access is access all Console_Type'Class;

   --  Report an error message.
   procedure Error (Console : in out Console_Type;
                    Message : in String) is abstract;

   --  Report a notice message.
   procedure Notice (Console : in out Console_Type;
                     Kind    : in Notice_Type;
                     Message : in String) is abstract;

   --  Print the field value for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in String;
                          Justify : in Justify_Type := J_LEFT) is abstract;

   --  Print the title for the given field.
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String) is abstract;

   --  Start a new title in a report.
   procedure Start_Title (Console : in out Console_Type) is abstract;

   --  Finish a new title in a report.
   procedure End_Title (Console : in out Console_Type) is abstract;

   --  Start a new row in a report.
   procedure Start_Row (Console : in out Console_Type) is abstract;

   --  Finish a new row in a report.
   procedure End_Row (Console : in out Console_Type) is abstract;

   --  Print the title for the given field and setup the associated field size.
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in String;
                          Length  : in Positive);

   --  Format the integer and print it for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Integer;
                          Justify : in Justify_Type := J_LEFT);

   --  Format the integer and print it for the given field.
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Ada.Strings.Unbounded.Unbounded_String;
                          Justify : in Justify_Type := J_LEFT);

   --  Get the field count that was setup through the Print_Title calls.
   function Get_Field_Count (Console : in Console_Type) return Natural;

   --  Reset the field count.
   procedure Clear_Fields (Console : in out Console_Type);

private

   type Field_Size_Array is array (Field_Type) of Natural;

   type Field_List_Array is array (1 .. Field_Size_Array'Length) of Field_Type;

   type Console_Type is abstract tagged limited record
      Sizes       : Field_Size_Array := (others => 0);
      Cols        : Field_Size_Array := (others => 1);
      Fields      : Field_List_Array;
      Field_Count : Natural := 0;
   end record;

end Util.Commands.Consoles;
