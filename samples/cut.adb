-----------------------------------------------------------------------
--  cut -- Text Transformations
--  Copyright (C) 2012, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Command_Line;
with Util.Strings;
with Util.Strings.Tokenizers;
procedure Cut is
   procedure Print_Token (Token : in String;
                          Done  : out Boolean);

   procedure Print_Token (Token : in String;
                          Done  : out Boolean) is
   begin
      Ada.Text_IO.Put_Line (Token);
      Done := False;
   end Print_Token;

   Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Count <= 1 then
      Ada.Text_IO.Put_Line ("Usage: cut pattern ...");
      Ada.Text_IO.Put_Line ("Example: cut : $PATH");
      return;
   end if;
   declare
      Pattern : constant String := Ada.Command_Line.Argument (1);
   begin
      for I in 2 .. Count loop
         Util.Strings.Tokenizers.Iterate_Tokens (Content => Ada.Command_Line.Argument (I),
                                                 Pattern => Pattern,
                                                 Process => Print_Token'Access);
      end loop;
   end;
end Cut;
