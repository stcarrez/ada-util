-----------------------------------------------------------------------
--  util-http-clients-curl -- HTTP Clients with CURL
--  Copyright (C) 2012, 2017, 2018, 2020, 2021, 2022 Stephane Carrez
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
with Util.Http.Mockups;
package Util.Http.Clients.Curl is

   --  Register the CURL Http manager.
   procedure Register;

private

   package C renames Interfaces.C;
   package Strings renames Interfaces.C.Strings;

   use type C.size_t;

   --  Define 'Int' and 'Chars_Ptr' with capitals to avoid GNAT warnings due
   --  to Eclipse capitalization.
   subtype Int is C.int;
   subtype Chars_Ptr is Strings.chars_ptr;
   subtype Size_T is C.size_t;

   subtype CURL is System.Address;

   type CURL_Code is new Interfaces.C.int;

   CURLE_OK               : constant CURL_Code := 0;

   type CURL_Info is new Int;

   type Curl_Option is new Int;

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
   overriding
   procedure Create (Manager  : in Curl_Http_Manager;
                     Http     : in out Client'Class);

   overriding
   procedure Do_Get (Manager  : in Curl_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class);

   overriding
   procedure Do_Head (Manager  : in Curl_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Reply    : out Response'Class);

   overriding
   procedure Do_Post (Manager  : in Curl_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class);

   overriding
   procedure Do_Put (Manager  : in Curl_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Data     : in String;
                     Reply    : out Response'Class);

   overriding
   procedure Do_Patch (Manager  : in Curl_Http_Manager;
                       Http     : in Client'Class;
                       URI      : in String;
                       Data     : in String;
                       Reply    : out Response'Class);

   overriding
   procedure Do_Delete (Manager  : in Curl_Http_Manager;
                        Http     : in Client'Class;
                        URI      : in String;
                        Reply    : out Response'Class);

   overriding
   procedure Do_Options (Manager  : in Curl_Http_Manager;
                         Http     : in Client'Class;
                         URI      : in String;
                         Reply    : out Response'Class);

   --  Set the timeout for the connection.
   overriding
   procedure Set_Timeout (Manager : in Curl_Http_Manager;
                          Http    : in Client'Class;
                          Timeout : in Duration);

   type Curl_Http_Request is new Util.Http.Mockups.Mockup_Request with record
      Data         : CURL := System.Null_Address;
      URL          : Chars_Ptr := Interfaces.C.Strings.Null_Ptr;
      Content      : Chars_Ptr := Interfaces.C.Strings.Null_Ptr;
      Curl_Headers : CURL_Slist_Access := null;
   end record;
   type Curl_Http_Request_Access is access all Curl_Http_Request'Class;

   --  Prepare to setup the headers in the request.
   procedure Set_Headers (Request : in out Curl_Http_Request);

   overriding
   procedure Finalize (Request : in out Curl_Http_Request);

   type Curl_Http_Response is new Util.Http.Mockups.Mockup_Response with record
      C : CURL;
      Parsing_Body : Boolean := False;
   end record;
   type Curl_Http_Response_Access is access all Curl_Http_Response'Class;

   --  Add a string to a CURL slist.
   function Curl_Slist_Append (List  : in CURL_Slist_Access;
                               Value : in Chars_Ptr) return CURL_Slist_Access;
   pragma Import (C, Curl_Slist_Append, "curl_slist_append");

   --  Free an entire CURL slist.
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
   function Curl_Easy_Setopt_Slist (Handle : in CURL;
                                     Option : in Curl_Option;
                                     Value  : in CURL_Slist_Access) return CURL_Code;
   pragma Import (C, Curl_Easy_Setopt_Slist, "curl_easy_setopt");

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

   type Write_Callback_Access is access
     function (Data  : in System.Address;
               Size  : in Size_T;
               Nmemb : in Size_T;
               Ptr   : in Curl_Http_Response_Access) return Size_T;
   pragma Convention (C, Write_Callback_Access);

   function Curl_Easy_Setopt_Write_Callback
     (Handle : in CURL;
      Option : in Curl_Option;
      Func   : in Write_Callback_Access)
      return CURL_Code;
   pragma Import (C, Curl_Easy_Setopt_Write_Callback, "curl_easy_setopt");

   --  Set options for a curl easy handle.
   function Curl_Easy_Setopt_Data (Handle : in CURL;
                                   Option : in Curl_Option;
                                   Value  : in Curl_Http_Response_Access) return CURL_Code;
   pragma Warnings (Off, Curl_Easy_Setopt_Data);
   pragma Import (C, Curl_Easy_Setopt_Data, "curl_easy_setopt");

   function Read_Response (Data     : in System.Address;
                           Size     : in Size_T;
                           Nmemb    : in Size_T;
                           Response : in Curl_Http_Response_Access) return Size_T;
   pragma Convention (C, Read_Response);

end Util.Http.Clients.Curl;
