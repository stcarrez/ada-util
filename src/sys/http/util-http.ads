-----------------------------------------------------------------------
--  util-http -- HTTP Utility Library
--  Copyright (C) 2012, 2018, 2022 Stephane Carrez
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
with Util.Blobs;

--  = HTTP =
--  The `Util.Http` package provides a set of APIs that allows applications to use
--  the HTTP protocol.  It defines a common interface on top of CURL and AWS so that
--  it is possible to use one of these two libraries in a transparent manner.
--
--  @include util-http-clients.ads
package Util.Http is

   --  Standard codes returned in HTTP responses.
   SC_CONTINUE                        : constant Natural := 100;
   SC_SWITCHING_PROTOCOLS             : constant Natural := 101;
   SC_OK                              : constant Natural := 200;
   SC_CREATED                         : constant Natural := 201;
   SC_ACCEPTED                        : constant Natural := 202;
   SC_NON_AUTHORITATIVE_INFORMATION   : constant Natural := 203;
   SC_NO_CONTENT                      : constant Natural := 204;
   SC_RESET_CONTENT                   : constant Natural := 205;
   SC_PARTIAL_CONTENT                 : constant Natural := 206;
   SC_MULTIPLE_CHOICES                : constant Natural := 300;
   SC_MOVED_PERMANENTLY               : constant Natural := 301;
   SC_MOVED_TEMPORARILY               : constant Natural := 302;
   SC_FOUND                           : constant Natural := 302;
   SC_SEE_OTHER                       : constant Natural := 303;
   SC_NOT_MODIFIED                    : constant Natural := 304;
   SC_USE_PROXY                       : constant Natural := 305;
   SC_TEMPORARY_REDIRECT              : constant Natural := 307;
   SC_BAD_REQUEST                     : constant Natural := 400;
   SC_UNAUTHORIZED                    : constant Natural := 401;
   SC_PAYMENT_REQUIRED                : constant Natural := 402;
   SC_FORBIDDEN                       : constant Natural := 403;
   SC_NOT_FOUND                       : constant Natural := 404;
   SC_METHOD_NOT_ALLOWED              : constant Natural := 405;
   SC_NOT_ACCEPTABLE                  : constant Natural := 406;
   SC_PROXY_AUTHENTICATION_REQUIRED   : constant Natural := 407;
   SC_REQUEST_TIMEOUT                 : constant Natural := 408;
   SC_CONFLICT                        : constant Natural := 409;
   SC_GONE                            : constant Natural := 410;
   SC_LENGTH_REQUIRED                 : constant Natural := 411;
   SC_PRECONDITION_FAILED             : constant Natural := 412;
   SC_REQUEST_ENTITY_TOO_LARGE        : constant Natural := 413;
   SC_REQUEST_URI_TOO_LONG            : constant Natural := 414;
   SC_UNSUPPORTED_MEDIA_TYPE          : constant Natural := 415;
   SC_REQUESTED_RANGE_NOT_SATISFIABLE : constant Natural := 416;
   SC_EXPECTATION_FAILED              : constant Natural := 417;
   SC_INTERNAL_SERVER_ERROR           : constant Natural := 500;
   SC_NOT_IMPLEMENTED                 : constant Natural := 501;
   SC_BAD_GATEWAY                     : constant Natural := 502;
   SC_SERVICE_UNAVAILABLE             : constant Natural := 503;
   SC_GATEWAY_TIMEOUT                 : constant Natural := 504;
   SC_HTTP_VERSION_NOT_SUPPORTED      : constant Natural := 505;

   --  ------------------------------
   --  Abstract Message
   --  ------------------------------
   --  The <b>Abstract_Message</b> interface describe an HTTP message representing either
   --  a request or a response.
   type Abstract_Message is limited interface;

   --  Returns a boolean indicating whether the named message header has already
   --  been set.
   function Contains_Header (Message : in Abstract_Message;
                             Name    : in String) return Boolean is abstract;

   --  Returns the value of the specified message header as a String. If the message
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any message header.
   function Get_Header (Message : in Abstract_Message;
                        Name    : in String) return String is abstract;

   --  Sets a message header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   procedure Set_Header (Message  : in out Abstract_Message;
                         Name     : in String;
                         Value    : in String) is abstract;

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   procedure Add_Header (Message : in out Abstract_Message;
                         Name    : in String;
                         Value   : in String) is abstract;

   --  Iterate over the message headers and executes the <b>Process</b> procedure.
   procedure Iterate_Headers (Message : in Abstract_Message;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is abstract;

   --  Sets a header with the given name and date-value.
   --  The date is specified in terms of milliseconds since the epoch.
   --  If the header had already been set, the new value overwrites the previous one.
   --  The containsHeader method can be used to test for the presence of a header
   --  before setting its value.
   procedure Set_Date_Header (Request  : in out Abstract_Message'Class;
                              Name     : in String;
                              Date     : in Ada.Calendar.Time);

   --  Adds a header with the given name and date-value. The date is specified
   --  in terms of milliseconds since the epoch. This method allows response headers
   --  to have multiple values.
   procedure Add_Date_Header (Request : in out Abstract_Message'Class;
                              Name    : in String;
                              Date    : in Ada.Calendar.Time);

   --  Sets a header with the given name and integer value.
   --  If the header had already been set, the new value overwrites the previous one.
   --  The containsHeader  method can be used to test for the presence of a header
   --  before setting its value.
   procedure Set_Int_Header (Request  : in out Abstract_Message'Class;
                             Name     : in String;
                             Value    : in Integer);

   --  Adds a header with the given name and integer value. This method
   --  allows headers to have multiple values.
   procedure Add_Int_Header (Request  : in out Abstract_Message'Class;
                             Name     : in String;
                             Value    : in Integer);

   --  ------------------------------
   --  Abstract Request
   --  ------------------------------
   type Abstract_Request is limited interface and Abstract_Message;
   type Abstract_Request_Access is access all Abstract_Request'Class;

   --  ------------------------------
   --  Abstract Response
   --  ------------------------------
   type Abstract_Response is limited interface and Abstract_Message;
   type Abstract_Response_Access is access all Abstract_Response'Class;

   --  Get the response status code.
   function Get_Status (Response : in Abstract_Response) return Natural is abstract;

   --  Get the response body as a string.
   function Get_Body (Response : in Abstract_Response) return String is abstract;

   --  Get the response body as a blob content.
   function Get_Body (Response : in Abstract_Response) return Util.Blobs.Blob_Ref is abstract;

end Util.Http;
