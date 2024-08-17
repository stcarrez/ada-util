-----------------------------------------------------------------------
--  encodes -- Encodes strings
--  Copyright (C) 2010 - 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Command_Line;
with Util.Encoders;

procedure Encodes is

   use Util.Encoders;

   Encode : Boolean := True;
   Count  : constant Natural := Ada.Command_Line.Argument_Count;

begin
   if Count <= 1 then
      Ada.Text_IO.Put_Line ("Usage: encodes {encoder} [-d|-e] string...");
      Ada.Text_IO.Put_Line ("Encoders: " & Util.Encoders.BASE_64 & ", "
                            & Util.Encoders.BASE_64_URL & ", "
                            & Util.Encoders.BASE_32 & ", "
                            & Util.Encoders.BASE_16 & ", "
                            & Util.Encoders.HASH_SHA1);
      return;
   end if;
   declare
      Name : constant String := Ada.Command_Line.Argument (1);
      C    : constant Encoder := Util.Encoders.Create (Name);
      D    : constant Decoder := Util.Encoders.Create (Name);
   begin
      for I in 2 .. Count loop
         declare
            S : constant String := Ada.Command_Line.Argument (I);
         begin
            if S = "-d" then
               Encode := False;
            elsif S = "-e" then
               Encode := True;
            elsif Encode then
               Ada.Text_IO.Put_Line ("Encodes " & Name & ": " & C.Encode (S));
            else
               Ada.Text_IO.Put_Line ("Decodes " & Name & ": " & D.Decode (S));
            end if;
         end;
      end loop;
   end;
end Encodes;
