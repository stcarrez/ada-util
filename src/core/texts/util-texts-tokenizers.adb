-----------------------------------------------------------------------
--  util-tests-tokenizers -- Split texts into tokens
--  Copyright (C) 2012, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Texts.Tokenizers is

   --  ------------------------------
   --  Iterate over the tokens of the <b>Content</b> input.  Each token is separated by
   --  a pattern represented by <b>Pattern</b>.  For each token, call the
   --  procedure <b>Process</b> with the token.  When <b>Going</b> is <b>Backward</b>,
   --  scan the input from the end.  Stop iterating over the tokens when the <b>Process</b>
   --  procedure returns True in <b>Done</b>.
   --  ------------------------------
   procedure Iterate_Tokens (Content   : in Input;
                             Pattern   : in Input;
                             Process   : access procedure (Token : in Input;
                                                           Done  : out Boolean);
                             Going     : in Ada.Strings.Direction := Ada.Strings.Forward) is
      use Ada.Strings;

      Sep_Pos : Natural;
      Pos     : Natural;
      Last    : constant Natural := Content'Last;
   begin
      case Going is
         when Forward =>
            Pos := Content'First;
            while Pos <= Last loop
               Sep_Pos := Index (Content, Pattern, Pos, Forward);
               if Sep_Pos = 0 then
                  Sep_Pos := Last;
               else
                  Sep_Pos := Sep_Pos - 1;
               end if;
               declare
                  Done : Boolean;
               begin
                  Process (Token => Content (Pos .. Sep_Pos), Done => Done);
                  exit when Done;
               end;
               Pos := Sep_Pos + 1 + Pattern'Length;
            end loop;

         when Backward =>
            Pos := Content'Last;
            while Pos >= Content'First loop
               Sep_Pos := Index (Content, Pattern, Pos, Backward);
               if Sep_Pos = 0 then
                  Sep_Pos := Content'First;
               else
                  Sep_Pos := Sep_Pos + 1;
               end if;
               declare
                  Done : Boolean;
               begin
                  Process (Token => Content (Sep_Pos .. Pos), Done => Done);
                  exit when Done or else Sep_Pos = Content'First;
               end;
               Pos := Sep_Pos - 1 - Pattern'Length;
            end loop;

      end case;
   end Iterate_Tokens;

end Util.Texts.Tokenizers;
