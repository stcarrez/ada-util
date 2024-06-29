-----------------------------------------------------------------------
--  util-http-clients-mockups -- HTTP Clients mockups
--  Copyright (C) 2011, 2012, 2017, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
package Util.Http.Clients.Mockups is

   --  Register the Http manager.
   procedure Register;

   --  Set the path of the file that contains the response for the next
   --  <b>Do_Get</b> and <b>Do_Post</b> calls.
   procedure Set_File (Path : in String);

private

   type File_Http_Manager is new Http_Manager with record
      File : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type File_Http_Manager_Access is access all File_Http_Manager'Class;

   overriding
   procedure Create (Manager  : in File_Http_Manager;
                     Http     : in out Client'Class);

   overriding
   procedure Do_Get (Manager  : in File_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class);

   overriding
   procedure Do_Head (Manager  : in File_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Reply    : out Response'Class);

   overriding
   procedure Do_Post (Manager  : in File_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class);

   overriding
   procedure Do_Put (Manager  : in File_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Data     : in String;
                     Reply    : out Response'Class);

   overriding
   procedure Do_Patch (Manager  : in File_Http_Manager;
                       Http     : in Client'Class;
                       URI      : in String;
                       Data     : in String;
                       Reply    : out Response'Class);

   overriding
   procedure Do_Delete (Manager  : in File_Http_Manager;
                        Http     : in Client'Class;
                        URI      : in String;
                        Reply    : out Response'Class);

   overriding
   procedure Do_Options (Manager  : in File_Http_Manager;
                         Http     : in Client'Class;
                         URI      : in String;
                         Reply    : out Response'Class);

   --  Set the timeout for the connection.
   overriding
   procedure Set_Timeout (Manager : in File_Http_Manager;
                          Http    : in Client'Class;
                          Timeout : in Duration);

end Util.Http.Clients.Mockups;
