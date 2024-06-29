-----------------------------------------------------------------------
--  util-commands-consoles-text -- Text console interface
--  Copyright (C) 2014, 2017, 2018, 2021, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
generic
   with package IO is new Util.Commands.IO (<>);
   with function To_String (Input : in Input_Type) return String is <>;
package Util.Commands.Consoles.Text is

   type Console_Type is new Util.Commands.Consoles.Console_Type with private;

   --  Report an error message.
   overriding
   procedure Error (Console : in out Console_Type;
                    Message : in Input_Type);

   --  Report a notice message.
   overriding
   procedure Notice (Console : in out Console_Type;
                     Kind    : in Notice_Type;
                     Message : in Input_Type);

   --  Print the field value for the given field.
   overriding
   procedure Print_Field (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Value   : in Input_Type;
                          Justify : in Justify_Type := J_LEFT);

   --  Print the title for the given field.
   overriding
   procedure Print_Title (Console : in out Console_Type;
                          Field   : in Field_Type;
                          Title   : in Input_Type;
                          Justify : in Justify_Type := J_LEFT);

   --  Start a new title in a report.
   overriding
   procedure Start_Title (Console : in out Console_Type);

   --  Finish a new title in a report.
   overriding
   procedure End_Title (Console : in out Console_Type);

   --  Start a new row in a report.
   overriding
   procedure Start_Row (Console : in out Console_Type);

   --  Finish a new row in a report.
   overriding
   procedure End_Row (Console : in out Console_Type);

private

   type Console_Type is new Util.Commands.Consoles.Console_Type with record
      Cur_Col : Positive := 1;
   end record;

   procedure Set_Col (Console : in out Console_Type;
                      Col     : in Positive);

   procedure Put (Console : in out Console_Type;
                  Content : in Input_Type);

end Util.Commands.Consoles.Text;
