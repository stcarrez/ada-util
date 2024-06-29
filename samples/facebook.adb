-----------------------------------------------------------------------
--  facebook -- Get information about a Facebook user using the Facebook API
--  Copyright (C) 2012, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;

with Util.Http.Clients.AWS;
with Util.Http.Rest;

with Mapping;

--  This example shows how to invoke a REST service, retrieve and extract a JSON content
--  into some Ada record.  It uses the Facebook Graph API which does not need any
--  authentication (ie, the public Facebook API).
procedure Facebook is

   procedure Print (P : in Mapping.Person);

   procedure Print (P : in Mapping.Person) is
      use Ada.Strings.Unbounded;
   begin
      Ada.Text_IO.Put_Line ("Id         : " & Long_Long_Integer'Image (P.Id));
      Ada.Text_IO.Put_Line ("Name       : " & To_String (P.Name));
      Ada.Text_IO.Put_Line ("First name : " & To_String (P.First_Name));
      Ada.Text_IO.Put_Line ("Last name  : " & To_String (P.Last_Name));
      Ada.Text_IO.Put_Line ("Username   : " & To_String (P.Username));
      Ada.Text_IO.Put_Line ("Gender     : " & To_String (P.Gender));
      Ada.Text_IO.Put_Line ("Link       : " & To_String (P.Link));
   end Print;

   procedure Get_User is new Util.Http.Rest.Rest_Get (Mapping.Person_Mapper);

   Count : constant Natural := Ada.Command_Line.Argument_Count;

   --  Mapping for the Person record.
   Person_Mapping  : aliased Mapping.Person_Mapper.Mapper;
begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: facebook username ...");
      Ada.Text_IO.Put_Line ("Example: facebook btaylor");
      return;
   end if;
   Person_Mapping.Add_Mapping ("id", Mapping.FIELD_ID);
   Person_Mapping.Add_Mapping ("name", Mapping.FIELD_NAME);
   Person_Mapping.Add_Mapping ("first_name", Mapping.FIELD_FIRST_NAME);
   Person_Mapping.Add_Mapping ("last_name", Mapping.FIELD_LAST_NAME);
   Person_Mapping.Add_Mapping ("link", Mapping.FIELD_LINK);
   Person_Mapping.Add_Mapping ("username", Mapping.FIELD_USER_NAME);
   Person_Mapping.Add_Mapping ("gender", Mapping.FIELD_GENDER);
   Util.Http.Clients.AWS.Register;
   for I in 1 .. Count loop
      declare
         URI      : constant String := Ada.Command_Line.Argument (I);
         P        : aliased Mapping.Person;
      begin
         Get_User (URI     => "https://graph.facebook.com/" & URI,
                   Mapping => Person_Mapping'Unchecked_Access,
                   Into    => P'Unchecked_Access);
         Print (P);
      end;
   end loop;
end Facebook;
