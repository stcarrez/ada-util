-----------------------------------------------------------------------
--  util-http-mockups -- Mockup implementations for HTTP requests and responses
--  Copyright (C) 2012, 2022 Stephane Carrez
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

with Ada.Finalization;
with Util.Strings.Maps;
with Util.Strings.Builders;
package Util.Http.Mockups is

   package AF renames Ada.Finalization;

   type Mockup_Message is new AF.Limited_Controlled and Abstract_Message with private;
   type Mockup_Message_Access is access all Mockup_Message'Class;

   --  Returns a boolean indicating whether the named request header has already
   --  been set.
   overriding
   function Contains_Header (Message : in Mockup_Message;
                             Name    : in String) return Boolean;

   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   overriding
   function Get_Header (Message : in Mockup_Message;
                        Name    : in String) return String;

   --  Sets a request header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Message : in out Mockup_Message;
                         Name    : in String;
                         Value : in String);

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   overriding
   procedure Add_Header (Message : in out Mockup_Message;
                         Name    : in String;
                         Value   : in String);

   --  Iterate over the message headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Message : in Mockup_Message;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   type Mockup_Request is new Mockup_Message and Abstract_Request with private;
   type Mockup_Request_Access is access all Mockup_Request'Class;

   type Mockup_Response is new Mockup_Message and Abstract_Response with private;
   type Mockup_Response_Access is access all Mockup_Response'Class;

   --  Get the response body as a string.
   overriding
   function Get_Body (Reply : in Mockup_Response) return String;

   --  Get the response body as a blob content.
   overriding
   function Get_Body (Reply : in Mockup_Response) return Util.Blobs.Blob_Ref;

   --  Get the response status code.
   overriding
   function Get_Status (Reply : in Mockup_Response) return Natural;

   --  Set the response status code.
   procedure Set_Status (Reply  : in out Mockup_Response;
                         Status : in Natural);

   --  Set the response body.
   procedure Set_Body (Reply   : in out Mockup_Response;
                       Content : in String);

   --  Append the content to the response body.
   procedure Append_Body (Reply   : in out Mockup_Response;
                          Content : in String);

private

   type Mockup_Message is new AF.Limited_Controlled and Abstract_Message with record
      Headers : Util.Strings.Maps.Map;
   end record;

   type Mockup_Request is new Mockup_Message and Abstract_Request with null record;

   type Mockup_Response is new Mockup_Message and Abstract_Response with record
      Content : Util.Strings.Builders.Builder (1024);
      Status  : Natural;
   end record;

end Util.Http.Mockups;
