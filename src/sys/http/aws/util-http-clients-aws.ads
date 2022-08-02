-----------------------------------------------------------------------
--  util-http-clients-web -- HTTP Clients with AWS implementation
--  Copyright (C) 2011, 2012, 2017, 2019, 2020, 2022 Stephane Carrez
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

private with Util.Http.Clients.AWS_I.Headers;
private with Util.Http.Clients.AWS_I.Response;
private with Util.Http.Clients.AWS_I.Client;
package Util.Http.Clients.AWS is

   --  Register the Http manager.
   procedure Register;

private

   type AWS_Http_Manager is new Http_Manager with record
      Timeout : Duration := 60.0;
   end record;
   type AWS_Http_Manager_Access is access all Http_Manager'Class;

   overriding
   procedure Create (Manager  : in AWS_Http_Manager;
                     Http     : in out Client'Class);

   overriding
   procedure Do_Get (Manager  : in AWS_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class);

   overriding
   procedure Do_Head (Manager  : in AWS_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Reply    : out Response'Class);

   overriding
   procedure Do_Post (Manager  : in AWS_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class);

   overriding
   procedure Do_Put (Manager  : in AWS_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Data     : in String;
                     Reply    : out Response'Class);

   overriding
   procedure Do_Patch (Manager  : in AWS_Http_Manager;
                       Http     : in Client'Class;
                       URI      : in String;
                       Data     : in String;
                       Reply    : out Response'Class);

   overriding
   procedure Do_Delete (Manager  : in AWS_Http_Manager;
                        Http     : in Client'Class;
                        URI      : in String;
                        Reply    : out Response'Class);

   overriding
   procedure Do_Options (Manager  : in AWS_Http_Manager;
                         Http     : in Client'Class;
                         URI      : in String;
                         Reply    : out Response'Class);

   --  Set the timeout for the connection.
   overriding
   procedure Set_Timeout (Manager : in AWS_Http_Manager;
                          Http    : in Client'Class;
                          Timeout : in Duration);

   type AWS_Http_Request is new Http_Request with record
      Timeouts : AWS_I.Client.Timeouts_Values := AWS_I.Client.No_Timeout;
      Headers  : AWS_I.Headers.List;
   end record;
   type AWS_Http_Request_Access is access all AWS_Http_Request'Class;

   --  Returns a boolean indicating whether the named request header has already
   --  been set.
   overriding
   function Contains_Header (Http : in AWS_Http_Request;
                             Name : in String) return Boolean;

   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   overriding
   function Get_Header (Request : in AWS_Http_Request;
                        Name    : in String) return String;

   --  Sets a request header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Http  : in out AWS_Http_Request;
                         Name  : in String;
                         Value : in String);

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   overriding
   procedure Add_Header (Http  : in out AWS_Http_Request;
                         Name  : in String;
                         Value : in String);

   --  Iterate over the request headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Request : in AWS_Http_Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   type AWS_Http_Response is new Http_Response with record
      Data : AWS_I.Response.Data;
   end record;
   type AWS_Http_Response_Access is access all AWS_Http_Response'Class;

   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   overriding
   function Contains_Header (Reply : in AWS_Http_Response;
                             Name  : in String) return Boolean;

   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   overriding
   function Get_Header (Reply  : in AWS_Http_Response;
                        Name   : in String) return String;

   --  Sets a message header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Reply    : in out AWS_Http_Response;
                         Name     : in String;
                         Value    : in String);

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   overriding
   procedure Add_Header (Reply   : in out AWS_Http_Response;
                         Name    : in String;
                         Value   : in String);

   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Reply   : in AWS_Http_Response;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   --  Get the response body as a string.
   overriding
   function Get_Body (Reply : in AWS_Http_Response) return String;

   overriding
   function Get_Body (Reply : in AWS_Http_Response) return Util.Blobs.Blob_Ref;

   --  Get the response status code.
   overriding
   function Get_Status (Reply : in AWS_Http_Response) return Natural;

end Util.Http.Clients.AWS;
