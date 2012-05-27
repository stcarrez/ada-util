-----------------------------------------------------------------------
--  util-http-clients -- HTTP Clients
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with Ada.Calendar;
with Ada.Finalization;

with Util.Http.Cookies;
package Util.Http.Clients is

   Connection_Error : exception;

   --  ------------------------------
   --  Http response
   --  ------------------------------
   --  The <b>Response</b> type represents a response returned by an HTTP request.
   type Response is limited new Abstract_Response with private;

   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   overriding
   function Contains_Header (Reply : in Response;
                             Name  : in String) return Boolean;

   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   overriding
   function Get_Header (Reply  : in Response;
                        Name   : in String) return String;

   --  Sets a message header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Reply    : in out Response;
                         Name     : in String;
                         Value    : in String);

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   overriding
   procedure Add_Header (Reply   : in out Response;
                         Name    : in String;
                         Value   : in String);

   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Reply   : in Response;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   --  Get the response body as a string.
   overriding
   function Get_Body (Reply : in Response) return String;

   --  Get the response status code.
   overriding
   function Get_Status (Reply : in Response) return Natural;

   --  ------------------------------
   --  Http client
   --  ------------------------------
   --  The <b>Client</b> type allows to execute HTTP GET/POST requests.
   type Client is limited new Abstract_Request with private;
   type Client_Access is access all Client;

   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   overriding
   function Contains_Header (Request : in Client;
                             Name    : in String) return Boolean;

   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   overriding
   function Get_Header (Request : in Client;
                        Name    : in String) return String;

   --  Sets a header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Request  : in out Client;
                         Name     : in String;
                         Value    : in String);

   --  Adds a header with the given name and value.
   --  This method allows headers to have multiple values.
   overriding
   procedure Add_Header (Request  : in out Client;
                         Name     : in String;
                         Value    : in String);

   --  Iterate over the request headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Request : in Client;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   --  Removes all headers with the given name.
   procedure Remove_Header (Request : in out Client;
                            Name    : in String);

   --  Adds the specified cookie to the request.  This method can be called multiple
   --  times to set more than one cookie.
   procedure Add_Cookie (Http   : in out Client;
                         Cookie : in Util.Http.Cookies.Cookie);

   --  Execute an http GET request on the given URL.  Additional request parameters,
   --  cookies and headers should have been set on the client object.
   procedure Get (Request  : in out Client;
                  URL      : in String;
                  Reply    : out Response'Class);

--     procedure Head (Http : in out Client;
--                     URI  : in String;
--                     Reply : out Response'Class);

   --  Execute an http POST request on the given URL.  Additional request parameters,
   --  cookies and headers should have been set on the client object.
--     procedure Put (Http  : in out Client;
--                    URI   : in String;
--                    Reply : out Response'Class);

private

   subtype Http_Request is Abstract_Request;
   subtype Http_Request_Access is Abstract_Request_Access;

   subtype Http_Response is Abstract_Response;
   subtype Http_Response_Access is Abstract_Response_Access;

   type Http_Manager is interface;
   type Http_Manager_Access is access all Http_Manager'Class;

   procedure Create (Manager  : in Http_Manager;
                     Http     : in out Client'Class) is abstract;

   procedure Do_Get (Manager  : in Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class) is abstract;

   procedure Do_Post (Manager  : in Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class) is abstract;

   Default_Http_Manager : Http_Manager_Access;

   type Response is new Ada.Finalization.Limited_Controlled and Abstract_Response with record
      Delegate : Abstract_Response_Access;
   end record;

   --  Free the resource used by the response.
   overriding
   procedure Finalize (Reply : in out Response);

   type Client is new Ada.Finalization.Limited_Controlled and Abstract_Request with record
      Manager  : Http_Manager_Access;
      Delegate : Http_Request_Access;
   end record;

   --  Initialize the client
   overriding
   procedure Initialize (Http : in out Client);

   overriding
   procedure Finalize (Http : in out Client);

end Util.Http.Clients;
