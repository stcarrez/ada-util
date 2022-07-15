-----------------------------------------------------------------------
--  util-http-headers -- HTTP Headers
--  Copyright (C) 2022 Stephane Carrez
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
with Ada.Strings.Fixed;

with Util.Strings.Tokenizers;
package body Util.Http.Headers is

   --  Split an accept like header into multiple tokens and a quality value.
   --  Invoke the `Process` procedure for each token.  Example:
   --
   --     Accept-Language: de, en;q=0.7, jp, fr;q=0.8, ru
   --
   --  The `Process` will be called for "de", "en" with quality 0.7,
   --  and "jp", "fr" with quality 0.8 and then "ru" with quality 1.0.
   procedure Split_Header (Header  : in String;
                           Process : access procedure (Item    : in String;
                                                       Quality : in Quality_Type)) is
      use Util.Strings;
      procedure Process_Token (Token : in String;
                               Done  : out Boolean);

      Quality : Quality_Type := 1.0;

      procedure Process_Token (Token : in String;
                               Done  : out Boolean) is
         Name : constant String := Ada.Strings.Fixed.Trim (Token, Ada.Strings.Both);
      begin
         Process (Name, Quality);
         Done := False;
      end Process_Token;

      Q, N, Pos  : Natural;
      Last    : Natural := Header'First;
   begin
      while Last < Header'Last loop
         Quality := 1.0;
         Pos := Index (Header, ';', Last);
         if Pos > 0 then
            N := Index (Header, ',', Pos);
            if N = 0 then
               N := Header'Last + 1;
            end if;
            Q := Pos + 1;
            while Q < N loop
               if Header (Q .. Q + 1) = "q=" then
                  begin
                     Quality := Quality_Type'Value (Header (Q + 2 .. N - 1));
                  exception
                     when others =>
                        null;
                  end;
                  exit;

               elsif Header (Q) /= ' ' then
                  exit;

               end if;
               Q := Q + 1;
            end loop;

            Util.Strings.Tokenizers.Iterate_Tokens (Content => Header (Last .. Pos - 1),
                                                    Pattern => ",",
                                                    Process => Process_Token'Access);
            Last := N + 1;
         else
            Util.Strings.Tokenizers.Iterate_Tokens (Content => Header (Last .. Header'Last),
                                                    Pattern => ",",
                                                    Process => Process_Token'Access);
            return;
         end if;
      end loop;
   end Split_Header;

end Util.Http.Headers;
