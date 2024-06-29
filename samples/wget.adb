-----------------------------------------------------------------------
--  wget -- A simple wget command to fetch a page
--  Copyright (C) 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Command_Line;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
procedure Wget is
   Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: wget url ...");
      Ada.Text_IO.Put_Line ("Example: wget http://www.adacore.com");
      return;
   end if;
   Util.Http.Clients.Curl.Register;
   for I in 1 .. Count loop
      declare
         Http     : Util.Http.Clients.Client;
         URI      : constant String := Ada.Command_Line.Argument (I);
         Response : Util.Http.Clients.Response;
      begin
         Http.Add_Header ("X-Requested-By", "wget");
         Http.Get (URI, Response);
         Ada.Text_IO.Put_Line ("Code: " & Natural'Image (Response.Get_Status));
         Ada.Text_IO.Put_Line (Response.Get_Body);
      end;
   end loop;
end Wget;
