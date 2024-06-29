-----------------------------------------------------------------------
--  util-http-headers -- HTTP Headers
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Fixed;

with Util.Strings.Tokenizers;
package body Util.Http.Headers is

   --  ------------------------------
   --  Split an accept like header into multiple tokens and a quality value.
   --  Invoke the `Process` procedure for each token.  Example:
   --
   --     Accept-Language: de, en;q=0.7, jp, fr;q=0.8, ru
   --
   --  The `Process` will be called for "de", "en" with quality 0.7,
   --  and "jp", "fr" with quality 0.8 and then "ru" with quality 1.0.
   --  ------------------------------
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

   --  ------------------------------
   --  Get the accepted media according to the `Accept` header value and a list
   --  of media/mime types.  The quality matching and wildcard are handled
   --  so that we return the best match.  With the following HTTP header:
   --
   --     Accept: image/*; q=0.2, image/jpeg
   --
   --  and if the mimes list contains `image/png` but not `image/jpeg`, the
   --  first one is returned.  If the list contains both, then `image/jpeg` is
   --  returned.
   --  ------------------------------
   function Get_Accepted (Header : in String;
                          Mimes  : in Util.Http.Mimes.Mime_List)
                         return Util.Http.Mimes.Mime_Access is

      procedure Process_Accept (Name    : in String;
                                Quality : in Quality_Type);

      Selected         : Util.Http.Mimes.Mime_Access;
      Selected_Quality : Quality_Type := 0.0;

      procedure Process_Accept (Name    : in String;
                                Quality : in Quality_Type) is
         Pos : Natural;
      begin
         if Quality > Selected_Quality then
            Pos := Util.Strings.Index (Name, '*');
            if Pos = 0 then
               for Mime of Mimes loop
                  if Name = Mime.all then
                     Selected := Mime;
                     Selected_Quality := Quality;
                     return;
                  end if;
               end loop;
            elsif Name = "*/*" then
               Selected := Mimes (Mimes'First);
               Selected_Quality := Quality;
               return;
            else
               Pos := Pos - 1;
               if Pos <= Name'First then
                  return;
               end if;
               if Name (Pos .. Name'Last) /= "/*" then
                  return;
               end if;
               for Mime of Mimes loop
                  if Util.Strings.Starts_With (Mime.all, Name (Name'First .. Pos)) then
                     Selected := Mime;
                     Selected_Quality := Quality;
                     return;
                  end if;
               end loop;
            end if;
         end if;
      end Process_Accept;

   begin
      if Mimes'Length > 0 then
         if Header'Length > 0 then
            Split_Header (Header, Process_Accept'Access);
         else
            Selected := Mimes (Mimes'First);
         end if;
      end if;
      return Selected;
   end Get_Accepted;

end Util.Http.Headers;
