-----------------------------------------------------------------------
--  asf-clients-web -- HTTP Clients with AWS implementation
--  Copyright (C) 2011 Stephane Carrez
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
with Util.Log.Loggers;

--  private with AWS.Headers;
--  private with AWS.Response;
package body Util.Http.Clients.Curl is

   use System;

   pragma Linker_Options ("-lcurl");

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Http.Clients.Curl");

   Manager : aliased Curl_Http_Manager;

   --  ------------------------------
   --  Register the CURL Http manager.
   --  ------------------------------
   procedure Register is
   begin
      Default_Http_Manager := Manager'Access;
   end Register;

   --  ------------------------------
   --  Check the CURL result code and report and exception and a log message if
   --  the CURL code indicates an error.
   --  ------------------------------
   procedure Check_Code (Code    : in CURL_Code;
                         Message : in String) is
   begin
      if Code /= CURLE_OK then
         declare
            Error : constant Interfaces.C.Strings.Chars_Ptr := Curl_Easy_Strerror (Code);
            Msg   : constant String := Interfaces.C.Strings.Value (Error);
         begin
            Log.Error ("{0}: {1}", Message, Msg);
            raise Connection_Error with Msg;
         end;
      end if;
   end Check_Code;

   --  ------------------------------
   --  Create a new HTTP request associated with the current request manager.
   --  ------------------------------
   procedure Create (Manager  : in Curl_Http_Manager;
                     Http     : in out Client'Class) is
      Request : Curl_Http_Request_Access;
      Data    : CURL;
   begin
      Data := Curl_Easy_Init;
      if Data = System.Null_Address then
         raise Storage_Error with "curl_easy_init cannot create the CURL instance";
      end if;
      Request := new Curl_Http_Request;
      Request.Data  := Data;
      Http.Delegate := Request.all'Access;
   end Create;

   function Get_Request (Http : in Client'Class) return Curl_Http_Request_Access is
   begin
      return Curl_Http_Request'Class (Http.Delegate.all)'Access;
   end Get_Request;

   function Read_Response (Data : in Interfaces.C.Strings.Chars_Ptr;
                           Size : in Interfaces.C.Size_T;
                           Nmemb : in Interfaces.C.Size_T;
                           Response : in Curl_Http_Response_Access) return Interfaces.C.Size_T;

   function Read_Response (Data  : in Interfaces.C.Strings.Chars_Ptr;
                           Size  : in Interfaces.C.Size_T;
                           Nmemb : in Interfaces.C.Size_T;
                           Response : in Curl_Http_Response_Access) return Interfaces.C.Size_T is
      use type Interfaces.C.Size_T;

      Total : Interfaces.C.Size_T := Size * Nmemb;
      Ptr   : Interfaces.C.Strings.Chars_Ptr := Data;
   begin
      Log.Info ("Read response, size {0} - {1}", Interfaces.C.Size_T'Image (Size),
                Interfaces.C.Size_T'Image (Nmemb));
      Ada.Strings.Unbounded.Append (Response.Content, Interfaces.C.Strings.Value (Data, Total));
      return Total;
   end Read_Response;

   procedure Do_Get (Manager  : in Curl_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class) is
      use Interfaces.C;

      Req      : constant Curl_Http_Request_Access := Get_Request (Http);
      Result   : CURL_Code;
      Response : Curl_Http_Response_Access;
      Status   : aliased C.long;
   begin
      Log.Info ("GET {0}", URI);

      Result := Curl_Easy_Setopt_Write_Callback (Req.Data,  CURLOPT_WRITEUNCTION,
                                                 Read_Response'Access);
      Check_Code (Result, "set callback");

      Interfaces.C.Strings.Free (Req.URL);
      Req.URL := Strings.New_String (URI);

      Result := Curl_Easy_Setopt_String (Req.Data, CURLOPT_URL, Req.URL);
      Check_Code (Result, "set url");

      Response := new Curl_Http_Response;
      Result := Curl_Easy_Setopt_Data (Req.Data, CURLOPT_WRITEDATA, Response);
      Reply.Delegate := Response.all'Access;

      Result := Curl_Easy_Perform (Req.Data);
      Check_Code (Result, "get request");

      Result := Curl_Easy_Getinfo_Long (Req.Data, CURLINFO_RESPONSE_CODE, Status'Access);
      Check_Code (Result, "get response code");
      Response.Status := Natural (Status);
   end Do_Get;

   procedure Do_Post (Manager  : in Curl_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class) is
   begin
      null;
   end Do_Post;

   overriding
   procedure Finalize (Request : in out Curl_Http_Request) is
      use type System.Address;
   begin
      if Request.Data /= System.Null_Address then
         Curl_Easy_Cleanup (Request.Data);
         Request.Data := System.Null_Address;
      end if;
      if Request.Headers /= System.Null_Address then
         Curl_Slist_Free_All (Request.Headers);
         Request.Headers := System.Null_Address;
      end if;
      Interfaces.C.Strings.Free (Request.URL);
   end Finalize;

   --  Returns a boolean indicating whether the named request header has already
   --  been set.
   function Contains_Header (Http : in Curl_Http_Request;
                             Name : in String) return Boolean is
   begin
      return False;
   end Contains_Header;

   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   overriding
   function Get_Header (Reply  : in Curl_Http_Request;
                        Name   : in String) return String is
   begin
      return "";
   end Get_Header;

   --  Sets a request header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   procedure Set_Header (Http  : in out Curl_Http_Request;
                         Name  : in String;
                         Value : in String) is
   begin
      null;
   end Set_Header;


   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   procedure Add_Header (Http  : in out Curl_Http_Request;
                         Name  : in String;
                         Value : in String) is
   begin
      null;
   end Add_Header;

   --  Iterate over the message headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Http : in Curl_Http_Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is
   begin
      null;
   end Iterate_Headers;

   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   function Contains_Header (Reply : in Curl_Http_Response;
                             Name  : in String) return Boolean is
   begin
      return False;
   end Contains_Header;

   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   function Get_Header (Reply  : in Curl_Http_Response;
                        Name   : in String) return String is
   begin
      return "";
   end Get_Header;

   --  Sets a message header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Reply    : in out Curl_Http_Response;
                         Name     : in String;
                         Value    : in String) is
   begin
      null;
   end Set_Header;

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   overriding
   procedure Add_Header (Reply   : in out Curl_Http_Response;
                         Name    : in String;
                         Value   : in String) is
   begin
      null;
   end Add_Header;

   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Reply   : in Curl_Http_Response;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is
   begin
      null;
   end Iterate_Headers;

   --  Get the response body as a string.
   function Get_Body (Reply : in Curl_Http_Response) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Reply.Content);
   end Get_Body;

   --  ------------------------------
   --  Get the response status code.
   --  ------------------------------
   overriding
   function Get_Status (Reply : in Curl_Http_Response) return Natural is
   begin
      return Reply.Status;
   end Get_Status;

end Util.Http.Clients.Curl;
