-----------------------------------------------------------------------
--  util-http-clients-mockups -- HTTP Clients
--  Copyright (C) 2011, 2012, 2017, 2020, 2021, 2022 Stephane Carrez
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

with Util.Files;
with Util.Http.Mockups;
package body Util.Http.Clients.Mockups is

   use Ada.Strings.Unbounded;

   Manager : aliased File_Http_Manager;

   --  ------------------------------
   --  Register the Http manager.
   --  ------------------------------
   procedure Register is
   begin
      Default_Http_Manager := Manager'Access;
   end Register;

   --  ------------------------------
   --  Set the path of the file that contains the response for the next
   --  <b>Do_Get</b> and <b>Do_Post</b> calls.
   --  ------------------------------
   procedure Set_File (Path : in String) is
   begin
      Manager.File := To_Unbounded_String (Path);
   end Set_File;

   overriding
   procedure Create (Manager  : in File_Http_Manager;
                     Http     : in out Client'Class) is
      pragma Unreferenced (Manager);
   begin
      Http.Delegate := new Util.Http.Mockups.Mockup_Request;
   end Create;

   overriding
   procedure Do_Get (Manager  : in File_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class) is
      pragma Unreferenced (Http, URI);

      Rep : constant Util.Http.Mockups.Mockup_Response_Access
        := new Util.Http.Mockups.Mockup_Response;
      Content : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Reply.Delegate := Rep.all'Access;
      Util.Files.Read_File (Path     => To_String (Manager.File),
                            Into     => Content,
                            Max_Size => 100000);
      Rep.Set_Body (To_String (Content));
      Rep.Set_Status (SC_OK);
   end Do_Get;

   overriding
   procedure Do_Head (Manager  : in File_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Reply    : out Response'Class) is
      pragma Unreferenced (Manager, Http, URI);

      Rep : constant Util.Http.Mockups.Mockup_Response_Access
        := new Util.Http.Mockups.Mockup_Response;
   begin
      Reply.Delegate := Rep.all'Access;
      Rep.Set_Body ("");
      Rep.Set_Status (SC_OK);
   end Do_Head;

   overriding
   procedure Do_Post (Manager  : in File_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class) is
      pragma Unreferenced (Data);
   begin
      Manager.Do_Get (Http, URI, Reply);
   end Do_Post;

   overriding
   procedure Do_Put (Manager  : in File_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Data     : in String;
                     Reply    : out Response'Class) is
      pragma Unreferenced (Data);
   begin
      Manager.Do_Get (Http, URI, Reply);
   end Do_Put;

   overriding
   procedure Do_Patch (Manager  : in File_Http_Manager;
                       Http     : in Client'Class;
                       URI      : in String;
                       Data     : in String;
                       Reply    : out Response'Class) is
      pragma Unreferenced (Data);
   begin
      Manager.Do_Get (Http, URI, Reply);
   end Do_Patch;

   overriding
   procedure Do_Delete (Manager  : in File_Http_Manager;
                        Http     : in Client'Class;
                        URI      : in String;
                        Reply    : out Response'Class) is
   begin
      Manager.Do_Get (Http, URI, Reply);
   end Do_Delete;

   overriding
   procedure Do_Options (Manager  : in File_Http_Manager;
                         Http     : in Client'Class;
                         URI      : in String;
                         Reply    : out Response'Class) is
   begin
      Manager.Do_Get (Http, URI, Reply);
   end Do_Options;

   --  ------------------------------
   --  Set the timeout for the connection.
   --  ------------------------------
   overriding
   procedure Set_Timeout (Manager : in File_Http_Manager;
                          Http    : in Client'Class;
                          Timeout : in Duration) is
      pragma Unreferenced (Manager, Http, Timeout);
   begin
      null;
   end Set_Timeout;

end Util.Http.Clients.Mockups;
