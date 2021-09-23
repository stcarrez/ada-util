-----------------------------------------------------------------------
--  wget -- A simple wget command to fetch a page
--  Copyright (C) 2012, 2013 Stephane Carrez
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
