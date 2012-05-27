-----------------------------------------------------------------------
--  util-http -- HTTP Utility Library
--  Copyright (C) 2012 Stephane Carrez
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
package Util.Http is

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

end Util.Http;
