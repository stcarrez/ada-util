-----------------------------------------------------------------------
--  util-tests-tokenizers -- Split texts into tokens
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings;
generic
   type Char is (<>);
   type Input is array (Positive range <>) of Char;
   with function Index (Item    : in Input;
                        Pattern : in Input;
                        From    : in Positive;
                        Going   : in Ada.Strings.Direction := Ada.Strings.Forward)
                        return Natural is <>;
package Util.Texts.Tokenizers is

   pragma Preelaborate;

   --  Iterate over the tokens of the <b>Content</b> input.  Each token is separated by
   --  a pattern represented by <b>Pattern</b>.  For each token, call the
   --  procedure <b>Process</b> with the token.  When <b>Going</b> is <b>Backward</b>,
   --  scan the input from the end.  Stop iterating over the tokens when the <b>Process</b>
   --  procedure returns True in <b>Done</b>.
   procedure Iterate_Tokens (Content   : in Input;
                             Pattern   : in Input;
                             Process   : access procedure (Token : in Input;
                                                           Done  : out Boolean);
                             Going     : in Ada.Strings.Direction := Ada.Strings.Forward);

end Util.Texts.Tokenizers;
