-----------------------------------------------------------------------
--  util-http-clients-curl -- HTTP Clients with CURL
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
--
with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Strings.Unbounded;
with Util.Strings.Maps;
package Util.Http.Clients.Curl is

   --  Register the CURL Http manager.
   procedure Register;

private

   package C renames Interfaces.C;
   package Strings renames Interfaces.C.Strings;

   use type C.size_t;
   use type C.int;

   --  Define 'Int' and 'Chars_Ptr' with capitals to avoid GNAT warnings due
   --  to Eclipse capitalization.
   subtype Int is C.int;
   subtype Chars_Ptr is Strings.chars_ptr;
   subtype Size_T is C.size_t;

   subtype CURL is System.Address;

   type CURL_Code is new Interfaces.C.int;

   CURLE_OK               : constant CURL_Code := 0;

   type CURL_Info is new Int;

   CURLINFO_RESPONSE_CODE : constant CURL_Info := 2097154;

   type Curl_Option is new Int;

   CURLOPT_URL            : constant Curl_Option := 10002;
   CURLOPT_READFUNCTION   : constant Curl_Option := 20012;
   CURLOPT_WRITEUNCTION   : constant Curl_Option := 20011;
   CURLOPT_HTTPHEADER     : constant Curl_Option := 10023;
   CURLOPT_INTERFACE      : constant Curl_Option := 10062;
   CURLOPT_USERPWD        : constant Curl_Option := 10005;
   CURLOPT_HTTPAUTH       : constant Curl_Option := 107;
   CURLOPT_MAXFILESIZE    : constant Curl_Option := 114;
   CURLOPT_WRITEDATA      : constant Curl_Option := 10001;
   CURLOPT_HEADER         : constant Curl_Option := 42;

   type CURL_Slist;
   type CURL_Slist_Access is access CURL_Slist;

   type CURL_Slist is record
      Data : Chars_Ptr;
      Next : CURL_Slist_Access;
   end record;

   --  Check the CURL result code and report and exception and a log message if
   --  the CURL code indicates an error.
   procedure Check_Code (Code    : in CURL_Code;
                         Message : in String);

   type Curl_Http_Manager is new Http_Manager with null record;
   type Curl_Http_Manager_Access is access all Http_Manager'Class;

   --  Create a new HTTP request associated with the current request manager.
   procedure Create (Manager  : in Curl_Http_Manager;
                     Http     : in out Client'Class);

   procedure Do_Get (Manager  : in Curl_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class);

   procedure Do_Post (Manager  : in Curl_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class);

   type Curl_Http_Request is new Ada.Finalization.Limited_Controlled and Http_Request with record
      Data    : CURL := System.Null_Address;
      URL     : Chars_Ptr := Interfaces.C.Strings.Null_Ptr;
      Headers : CURL_Slist_Access := null;
   end record;
   type Curl_Http_Request_Access is access all Curl_Http_Request'Class;

   overriding
   procedure Finalize (Request : in out Curl_Http_Request);

   --  Returns a boolean indicating whether the named request header has already
   --  been set.
   overriding
   function Contains_Header (Http : in Curl_Http_Request;
                             Name : in String) return Boolean;

   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   overriding
   function Get_Header (Reply  : in Curl_Http_Request;
                        Name   : in String) return String;

   --  Sets a request header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Http  : in out Curl_Http_Request;
                         Name  : in String;
                         Value : in String);

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   overriding
   procedure Add_Header (Http  : in out Curl_Http_Request;
                         Name  : in String;
                         Value : in String);

   --  Iterate over the message headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Http : in Curl_Http_Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   type Curl_Http_Response is new Ada.Finalization.Limited_Controlled and Http_Response with record
      C : CURL;
      Content : Ada.Strings.Unbounded.Unbounded_String;
      Status  : Natural;
      Parsing_Body : Boolean := False;
      Headers : Util.Strings.Maps.Map;
   end record;
   type Curl_Http_Response_Access is access all Curl_Http_Response'Class;

   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   overriding
   function Contains_Header (Reply : in Curl_Http_Response;
                             Name  : in String) return Boolean;

   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   overriding
   function Get_Header (Reply  : in Curl_Http_Response;
                        Name   : in String) return String;

   --  Sets a message header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Reply    : in out Curl_Http_Response;
                         Name     : in String;
                         Value    : in String);

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   overriding
   procedure Add_Header (Reply   : in out Curl_Http_Response;
                         Name    : in String;
                         Value   : in String);

   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Reply   : in Curl_Http_Response;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   --  Get the response body as a string.
   overriding
   function Get_Body (Reply : in Curl_Http_Response) return String;

   --  Get the response status code.
   overriding
   function Get_Status (Reply : in Curl_Http_Response) return Natural;

   --  Add a string to a CURL slist.
   function Curl_Slist_Append (List  : in CURL_Slist_Access;
                               Value : in Chars_Ptr) return CURL_Slist_Access;
   pragma Import (C, Curl_Slist_Append, "curl_slist_append");

   --  Free an entrire CURL slist.
   procedure Curl_Slist_Free_All (List : in CURL_Slist_Access);
   pragma Import (C, Curl_Slist_Free_All, "curl_slist_free_all");

   --  Start a libcurl easy session.
   function Curl_Easy_Init return CURL;
   pragma Import (C, Curl_Easy_Init, "curl_easy_init");

   --  End a libcurl easy session.
   procedure Curl_Easy_Cleanup (Handle : in CURL);
   pragma Import (C, Curl_Easy_Cleanup, "curl_easy_cleanup");

   --  Perform a file transfer.
   function Curl_Easy_Perform (Handle : in CURL) return CURL_Code;
   pragma Import (C, Curl_Easy_Perform, "curl_easy_perform");

   --  Return the error message which correspond to the given CURL error code.
   function Curl_Easy_Strerror (Code : in CURL_Code) return Chars_Ptr;
   pragma Import (C, Curl_Easy_Strerror, "curl_easy_strerror");

   --  Set options for a curl easy handle.
   function Curl_Easy_Setopt_String (Handle : in CURL;
                                     Option : in Curl_Option;
                                     Value  : in Chars_Ptr) return CURL_Code;
   pragma Import (C, Curl_Easy_Setopt_String, "curl_easy_setopt");

   --  Set options for a curl easy handle.
   function Curl_Easy_Setopt_Long (Handle : in CURL;
                                     Option : in Curl_Option;
                                     Value  : in Interfaces.C.long) return CURL_Code;
   pragma Import (C, Curl_Easy_Setopt_Long, "curl_easy_setopt");

   --  Get information from a CURL handle for an option returning a String.
   function Curl_Easy_Getinfo_String (Handle : in CURL;
                                      Option : in CURL_Info;
                                      Value  : access Chars_Ptr) return CURL_Code;
   pragma Import (C, Curl_Easy_Getinfo_String, "curl_easy_getinfo");

   --  Get information from a CURL handle for an option returning a Long.
   function Curl_Easy_Getinfo_Long (Handle : in CURL;
                                    Option : in CURL_Info;
                                    Value  : access C.long) return CURL_Code;
   pragma Import (C, Curl_Easy_Getinfo_Long, "curl_easy_getinfo");

   function Curl_Easy_Setopt_Write_Callback
     (Handle : in CURL;
      Option : in Curl_Option;
      Func : access function (Data  : in Chars_Ptr;
                              Size  : in Size_T;
                              Nmemb : in Size_T;
                              Ptr   : in Curl_Http_Response_Access) return Size_T)
      return CURL_Code;
   pragma Import (C, Curl_Easy_Setopt_Write_Callback, "curl_easy_setopt");

   --  Set options for a curl easy handle.
   function Curl_Easy_Setopt_Data (Handle : in CURL;
                                   Option : in Curl_Option;
                                   Value  : in Curl_Http_Response_Access) return CURL_Code;
   pragma Import (C, Curl_Easy_Setopt_Data, "curl_easy_setopt");

   function Read_Response (Data     : in Chars_Ptr;
                           Size     : in Size_T;
                           Nmemb    : in Size_T;
                           Response : in Curl_Http_Response_Access) return Size_T;

end Util.Http.Clients.Curl;
