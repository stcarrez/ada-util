-----------------------------------------------------------------------
--  Util-texts-formats -- Text Format ala Java MessageFormat
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2022 Stephane Carrez
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

package body Util.Texts.Formats is

   type Code is mod 2**32;

   --  ------------------------------
   --  Format the message and replace occurrences of argument patterns by
   --  their associated value.
   --  Returns the formatted message in the stream
   --  ------------------------------
   procedure Format (Message   : in Input;
                     Arguments : in Value_List;
                     Into      : in out Stream) is
      C : Code;
      Old_Pos : Natural;
      N       : Natural;
      Pos     : Natural := Message'First;
   begin
      while Pos <= Message'Last loop
         C := Char'Pos (Message (Pos));
         if C = Character'Pos ('{') then
            N := 0;
            Pos := Pos + 1;
            Old_Pos := Pos;
            while Pos <= Message'Last loop
               C := Char'Pos (Message (Pos));
               if C >= Character'Pos ('0') and then C <= Character'Pos ('9') then
                  N := N * 10 + Natural (C - Character'Pos ('0'));
                  Pos := Pos + 1;
               elsif C = Character'Pos ('}') then
                  if N >= Arguments'Length then
                     Put (Into, '{');
                     Pos := Old_Pos;
                  else
                     Format (Arguments (N + Arguments'First), Into);
                     Pos := Pos + 1;
                  end if;
                  exit;
               else
                  Put (Into, '{');
                  Pos := Old_Pos;
                  exit;
               end if;
            end loop;
         else
            Put (Into, Character'Val (C));
            Pos := Pos + 1;
         end if;
      end loop;
   end Format;

   procedure Format (Argument : in Value;
                     Into     : in out Stream) is
      Content : constant Input := To_Input (Argument);
      C       : Code;
   begin
      for I in Content'Range loop
         C := Char'Pos (Content (I));
         Put (Into, Character'Val (C));
      end loop;
   end Format;

end Util.Texts.Formats;
