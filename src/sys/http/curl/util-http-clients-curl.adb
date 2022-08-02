-----------------------------------------------------------------------
--  util-http-clients-curl -- HTTP Clients with CURL
--  Copyright (C) 2012, 2017, 2018, 2020, 2022 Stephane Carrez
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

with Util.Strings;
with Util.Log.Loggers;
with Util.Http.Clients.Curl.Constants;
package body Util.Http.Clients.Curl is

   use System;

   pragma Linker_Options ("-lcurl");

   Log   : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Http.Clients.Curl");

   function Get_Request (Http : in Client'Class) return Curl_Http_Request_Access;

   PUT_TOKEN     : constant Chars_Ptr := Strings.New_String ("PUT");
   PATCH_TOKEN   : constant Chars_Ptr := Strings.New_String ("PATCH");
   OPTIONS_TOKEN : constant Chars_Ptr := Strings.New_String ("OPTIONS");
   DELETE_TOKEN  : constant Chars_Ptr := Strings.New_String ("DELETE");
   Manager       : aliased Curl_Http_Manager;

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
            Error : constant Chars_Ptr := Curl_Easy_Strerror (Code);
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
   overriding
   procedure Create (Manager  : in Curl_Http_Manager;
                     Http     : in out Client'Class) is
      pragma Unreferenced (Manager);

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

   --  ------------------------------
   --  This function is called by CURL when a response line was read.
   --  ------------------------------
   function Read_Response (Data     : in System.Address;
                           Size     : in Size_T;
                           Nmemb    : in Size_T;
                           Response : in Curl_Http_Response_Access) return Size_T is

      Total : constant Size_T := Size * Nmemb;
      Last  : Natural;
      Line  : String (1 .. Natural (Total));
      for Line'Address use Data;
   begin
      if Response.Parsing_Body then
         Response.Append_Body (Line);

      elsif Total = 2 and then Line (1) = ASCII.CR and then Line (2) = ASCII.LF then
         Response.Parsing_Body := True;

      else
         Last := Line'Last;
         while Last > Line'First and then (Line (Last) = ASCII.CR or else Line (Last) = ASCII.LF) loop
            Last := Last - 1;
         end loop;
         Log.Debug ("RCV: {0}", Line (Line'First .. Last));
         declare
            Pos   : constant Natural := Util.Strings.Index (Line, ':');
            Start : Natural;
         begin
            if Pos > 0 then
               Start := Pos + 1;
               while Start <= Line'Last and then Line (Start) = ' ' loop
                  Start := Start + 1;
               end loop;
               Response.Add_Header (Name  => Line (Line'First .. Pos - 1),
                                    Value => Line (Start .. Last));
            end if;
         end;
      end if;
      return Total;
   end Read_Response;

   --  ------------------------------
   --  Prepare to setup the headers in the request.
   --  ------------------------------
   procedure Set_Headers (Request : in out Curl_Http_Request) is
      procedure Process (Name, Value : in String);

      procedure Process (Name, Value : in String) is
         S : Chars_Ptr := Strings.New_String (Name & ": " & Value);
      begin
         Request.Curl_Headers := Curl_Slist_Append (Request.Curl_Headers, S);
         Interfaces.C.Strings.Free (S);
      end Process;

   begin
      if Request.Curl_Headers /= null then
         Curl_Slist_Free_All (Request.Curl_Headers);
         Request.Curl_Headers := null;
      end if;

      Request.Iterate_Headers (Process'Access);
   end Set_Headers;

   overriding
   procedure Do_Get (Manager  : in Curl_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class) is
      pragma Unreferenced (Manager);
      use Interfaces.C;

      Req      : constant Curl_Http_Request_Access := Get_Request (Http);
      Result   : CURL_Code;
      Response : Curl_Http_Response_Access;
      Status   : aliased C.long;
   begin
      Log.Info ("GET {0}", URI);
      Result := Curl_Easy_Setopt_Write_Callback (Req.Data, Constants.CURLOPT_WRITEUNCTION,
                                                 Read_Response'Access);
      Check_Code (Result, "set callback");

      Req.Set_Headers;

      Interfaces.C.Strings.Free (Req.URL);
      Req.URL := Strings.New_String (URI);

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_HTTPGET, 1);
      Check_Code (Result, "set http GET");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_HEADER, 1);
      Check_Code (Result, "set header");

      Result := Curl_Easy_Setopt_Slist (Req.Data, Constants.CURLOPT_HTTPHEADER, Req.Curl_Headers);
      Check_Code (Result, "set http GET headers");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_URL, Req.URL);
      Check_Code (Result, "set url");

      Response := new Curl_Http_Response;
      Result := Curl_Easy_Setopt_Data (Req.Data, Constants.CURLOPT_WRITEDATA, Response);
      Check_Code (Result, "set write data");
      Reply.Delegate := Response.all'Access;

      Result := Curl_Easy_Perform (Req.Data);
      Check_Code (Result, "get request");

      Result := Curl_Easy_Getinfo_Long (Req.Data, Constants.CURLINFO_RESPONSE_CODE, Status'Access);
      Check_Code (Result, "get response code");
      Response.Set_Status (Natural (Status));
   end Do_Get;

   overriding
   procedure Do_Head (Manager  : in Curl_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Reply    : out Response'Class) is
      pragma Unreferenced (Manager);
      use Interfaces.C;

      Req      : constant Curl_Http_Request_Access := Get_Request (Http);
      Result   : CURL_Code;
      Response : Curl_Http_Response_Access;
      Status   : aliased C.long;
   begin
      Log.Info ("HEAD {0}", URI);
      Result := Curl_Easy_Setopt_Write_Callback (Req.Data, Constants.CURLOPT_WRITEUNCTION,
                                                 Read_Response'Access);
      Check_Code (Result, "set callback");

      Req.Set_Headers;

      Interfaces.C.Strings.Free (Req.URL);
      Req.URL := Strings.New_String (URI);

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_NOBODY, 1);
      Check_Code (Result, "set http HEAD");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_HEADER, 1);
      Check_Code (Result, "set header");

      Result := Curl_Easy_Setopt_Slist (Req.Data, Constants.CURLOPT_HTTPHEADER, Req.Curl_Headers);
      Check_Code (Result, "set http GET headers");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_URL, Req.URL);
      Check_Code (Result, "set url");

      Response := new Curl_Http_Response;
      Result := Curl_Easy_Setopt_Data (Req.Data, Constants.CURLOPT_WRITEDATA, Response);
      Check_Code (Result, "set write data");
      Reply.Delegate := Response.all'Access;

      Result := Curl_Easy_Perform (Req.Data);
      Check_Code (Result, "get request");

      Result := Curl_Easy_Getinfo_Long (Req.Data, Constants.CURLINFO_RESPONSE_CODE, Status'Access);
      Check_Code (Result, "get response code");
      Response.Set_Status (Natural (Status));
   end Do_Head;

   overriding
   procedure Do_Post (Manager  : in Curl_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class) is
      pragma Unreferenced (Manager);
      use Interfaces.C;

      Req      : constant Curl_Http_Request_Access := Get_Request (Http);
      Result   : CURL_Code;
      Response : Curl_Http_Response_Access;
      Status   : aliased C.long;
   begin
      Log.Info ("POST {0}", URI);

      Result := Curl_Easy_Setopt_Write_Callback (Req.Data, Constants.CURLOPT_WRITEUNCTION,
                                                 Read_Response'Access);
      Check_Code (Result, "set callback");
      Req.Set_Headers;

      Interfaces.C.Strings.Free (Req.URL);
      Req.URL := Strings.New_String (URI);

      Interfaces.C.Strings.Free (Req.Content);
      Req.Content := Strings.New_String (Data);

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POST, 1);
      Check_Code (Result, "set http POST");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_HEADER, 1);
      Check_Code (Result, "set header");

      Result := Curl_Easy_Setopt_Slist (Req.Data, Constants.CURLOPT_HTTPHEADER, Req.Curl_Headers);
      Check_Code (Result, "set http POST headers");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_URL, Req.URL);
      Check_Code (Result, "set url");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_POSTFIELDS, Req.Content);
      Check_Code (Result, "set post data");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POSTFIELDSIZE, Data'Length);
      Check_Code (Result, "set post data");

      Response := new Curl_Http_Response;
      Result := Curl_Easy_Setopt_Data (Req.Data, Constants.CURLOPT_WRITEDATA, Response);
      Check_Code (Result, "set write data");
      Reply.Delegate := Response.all'Access;

      Result := Curl_Easy_Perform (Req.Data);
      Check_Code (Result, "get request");

      Result := Curl_Easy_Getinfo_Long (Req.Data, Constants.CURLINFO_RESPONSE_CODE, Status'Access);
      Check_Code (Result, "get response code");
      Response.Set_Status (Natural (Status));
      if Req.Curl_Headers /= null then
         Curl_Slist_Free_All (Req.Curl_Headers);
         Req.Curl_Headers := null;
      end if;
   end Do_Post;

   overriding
   procedure Do_Put (Manager  : in Curl_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Data     : in String;
                     Reply    : out Response'Class) is
      pragma Unreferenced (Manager);
      use Interfaces.C;

      Req      : constant Curl_Http_Request_Access := Get_Request (Http);
      Result   : CURL_Code;
      Response : Curl_Http_Response_Access;
      Status   : aliased C.long;
   begin
      Log.Info ("PUT {0}", URI);

      Result := Curl_Easy_Setopt_Write_Callback (Req.Data, Constants.CURLOPT_WRITEUNCTION,
                                                 Read_Response'Access);
      Check_Code (Result, "set callback");
      Req.Set_Headers;

      Interfaces.C.Strings.Free (Req.URL);
      Req.URL := Strings.New_String (URI);

      Interfaces.C.Strings.Free (Req.Content);
      Req.Content := Strings.New_String (Data);

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POST, 1);
      Check_Code (Result, "set http PUT");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_HEADER, 1);
      Check_Code (Result, "set header");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_CUSTOMREQUEST, PUT_TOKEN);
      Check_Code (Result, "set http PUT");

      Result := Curl_Easy_Setopt_Slist (Req.Data, Constants.CURLOPT_HTTPHEADER, Req.Curl_Headers);
      Check_Code (Result, "set http PUT headers");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_URL, Req.URL);
      Check_Code (Result, "set url");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_POSTFIELDS, Req.Content);
      Check_Code (Result, "set put data");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POSTFIELDSIZE, Data'Length);
      Check_Code (Result, "set put data");

      Response := new Curl_Http_Response;
      Result := Curl_Easy_Setopt_Data (Req.Data, Constants.CURLOPT_WRITEDATA, Response);
      Check_Code (Result, "set write data");
      Reply.Delegate := Response.all'Access;

      Result := Curl_Easy_Perform (Req.Data);
      Check_Code (Result, "get request");

      Result := Curl_Easy_Getinfo_Long (Req.Data, Constants.CURLINFO_RESPONSE_CODE, Status'Access);
      Check_Code (Result, "get response code");
      Response.Set_Status (Natural (Status));

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_CUSTOMREQUEST,
                                         Interfaces.C.Strings.Null_Ptr);
      Check_Code (Result, "restore set http default");
      if Req.Curl_Headers /= null then
         Curl_Slist_Free_All (Req.Curl_Headers);
         Req.Curl_Headers := null;
      end if;
   end Do_Put;

   overriding
   procedure Do_Patch (Manager  : in Curl_Http_Manager;
                       Http     : in Client'Class;
                       URI      : in String;
                       Data     : in String;
                       Reply    : out Response'Class) is
      pragma Unreferenced (Manager);
      use Interfaces.C;

      Req      : constant Curl_Http_Request_Access := Get_Request (Http);
      Result   : CURL_Code;
      Response : Curl_Http_Response_Access;
      Status   : aliased C.long;
   begin
      Log.Info ("PATCH {0}", URI);

      Result := Curl_Easy_Setopt_Write_Callback (Req.Data, Constants.CURLOPT_WRITEUNCTION,
                                                 Read_Response'Access);
      Check_Code (Result, "set callback");
      Req.Set_Headers;

      Interfaces.C.Strings.Free (Req.URL);
      Req.URL := Strings.New_String (URI);

      Interfaces.C.Strings.Free (Req.Content);
      Req.Content := Strings.New_String (Data);

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POST, 1);
      Check_Code (Result, "set http PATCH");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_HEADER, 1);
      Check_Code (Result, "set header");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_CUSTOMREQUEST, PATCH_TOKEN);
      Check_Code (Result, "set http PATCH");

      Result := Curl_Easy_Setopt_Slist (Req.Data, Constants.CURLOPT_HTTPHEADER, Req.Curl_Headers);
      Check_Code (Result, "set http PATCH headers");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_URL, Req.URL);
      Check_Code (Result, "set url");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_POSTFIELDS, Req.Content);
      Check_Code (Result, "set patch data");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POSTFIELDSIZE, Data'Length);
      Check_Code (Result, "set patch data");

      Response := new Curl_Http_Response;
      Result := Curl_Easy_Setopt_Data (Req.Data, Constants.CURLOPT_WRITEDATA, Response);
      Check_Code (Result, "set write data");
      Reply.Delegate := Response.all'Access;

      Result := Curl_Easy_Perform (Req.Data);
      Check_Code (Result, "get request");

      Result := Curl_Easy_Getinfo_Long (Req.Data, Constants.CURLINFO_RESPONSE_CODE, Status'Access);
      Check_Code (Result, "get response code");
      Response.Set_Status (Natural (Status));

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_CUSTOMREQUEST,
                                         Interfaces.C.Strings.Null_Ptr);
      Check_Code (Result, "restore set http default");
      if Req.Curl_Headers /= null then
         Curl_Slist_Free_All (Req.Curl_Headers);
         Req.Curl_Headers := null;
      end if;
   end Do_Patch;

   overriding
   procedure Do_Delete (Manager  : in Curl_Http_Manager;
                        Http     : in Client'Class;
                        URI      : in String;
                        Reply    : out Response'Class) is
      pragma Unreferenced (Manager);
      use Interfaces.C;

      Req      : constant Curl_Http_Request_Access := Get_Request (Http);
      Result   : CURL_Code;
      Response : Curl_Http_Response_Access;
      Status   : aliased C.long;
   begin
      Log.Info ("DELETE {0}", URI);

      Result := Curl_Easy_Setopt_Write_Callback (Req.Data, Constants.CURLOPT_WRITEUNCTION,
                                                 Read_Response'Access);
      Check_Code (Result, "set callback");
      Req.Set_Headers;

      Interfaces.C.Strings.Free (Req.URL);
      Req.URL := Strings.New_String (URI);

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POST, 1);
      Check_Code (Result, "set http DELETE");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_HEADER, 1);
      Check_Code (Result, "set header");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_CUSTOMREQUEST, DELETE_TOKEN);
      Check_Code (Result, "set http DELETE");

      Result := Curl_Easy_Setopt_Slist (Req.Data, Constants.CURLOPT_HTTPHEADER, Req.Curl_Headers);
      Check_Code (Result, "set http GET headers");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_URL, Req.URL);
      Check_Code (Result, "set url");

--        Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_POSTFIELDS, Req.Content);
--        Check_Code (Result, "set post data");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POSTFIELDSIZE, 0);
      Check_Code (Result, "set post data");

      Response := new Curl_Http_Response;
      Result := Curl_Easy_Setopt_Data (Req.Data, Constants.CURLOPT_WRITEDATA, Response);
      Check_Code (Result, "set write data");
      Reply.Delegate := Response.all'Access;

      Result := Curl_Easy_Perform (Req.Data);
      Check_Code (Result, "get request");

      Result := Curl_Easy_Getinfo_Long (Req.Data, Constants.CURLINFO_RESPONSE_CODE, Status'Access);
      Check_Code (Result, "get response code");
      Response.Set_Status (Natural (Status));

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_CUSTOMREQUEST,
                                         Interfaces.C.Strings.Null_Ptr);
      Check_Code (Result, "restore set http default");
   end Do_Delete;

   overriding
   procedure Do_Options (Manager  : in Curl_Http_Manager;
                         Http     : in Client'Class;
                         URI      : in String;
                         Reply    : out Response'Class) is
      pragma Unreferenced (Manager);
      use Interfaces.C;

      Req      : constant Curl_Http_Request_Access := Get_Request (Http);
      Result   : CURL_Code;
      Response : Curl_Http_Response_Access;
      Status   : aliased C.long;
   begin
      Log.Info ("OPTIONS {0}", URI);

      Result := Curl_Easy_Setopt_Write_Callback (Req.Data, Constants.CURLOPT_WRITEUNCTION,
                                                 Read_Response'Access);
      Check_Code (Result, "set callback");
      Req.Set_Headers;

      Interfaces.C.Strings.Free (Req.URL);
      Req.URL := Strings.New_String (URI);

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POST, 1);
      Check_Code (Result, "set http OPTIONS");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_HEADER, 1);
      Check_Code (Result, "set header");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_CUSTOMREQUEST, OPTIONS_TOKEN);
      Check_Code (Result, "set http OPTIONS");

      Result := Curl_Easy_Setopt_Slist (Req.Data, Constants.CURLOPT_HTTPHEADER, Req.Curl_Headers);
      Check_Code (Result, "set http OPTIONS headers");

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_URL, Req.URL);
      Check_Code (Result, "set url");

      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_POSTFIELDSIZE, 0);
      Check_Code (Result, "set options data");

      Response := new Curl_Http_Response;
      Result := Curl_Easy_Setopt_Data (Req.Data, Constants.CURLOPT_WRITEDATA, Response);
      Check_Code (Result, "set write data");
      Reply.Delegate := Response.all'Access;

      Result := Curl_Easy_Perform (Req.Data);
      Check_Code (Result, "get request");

      Result := Curl_Easy_Getinfo_Long (Req.Data, Constants.CURLINFO_RESPONSE_CODE, Status'Access);
      Check_Code (Result, "get response code");
      Response.Set_Status (Natural (Status));

      Result := Curl_Easy_Setopt_String (Req.Data, Constants.CURLOPT_CUSTOMREQUEST,
                                         Interfaces.C.Strings.Null_Ptr);
      Check_Code (Result, "restore set http default");
   end Do_Options;

   --  ------------------------------
   --  Set the timeout for the connection.
   --  ------------------------------
   overriding
   procedure Set_Timeout (Manager : in Curl_Http_Manager;
                          Http    : in Client'Class;
                          Timeout : in Duration) is
      pragma Unreferenced (Manager);

      Req      : constant Curl_Http_Request_Access := Get_Request (Http);
      Time     : constant Interfaces.C.long := Interfaces.C.long (Timeout);
      Result   : CURL_Code;
   begin
      Result := Curl_Easy_Setopt_Long (Req.Data, Constants.CURLOPT_TIMEOUT, Time);
      Check_Code (Result, "set timeout");
   end Set_Timeout;

   overriding
   procedure Finalize (Request : in out Curl_Http_Request) is
   begin
      if Request.Data /= System.Null_Address then
         Curl_Easy_Cleanup (Request.Data);
         Request.Data := System.Null_Address;
      end if;
      if Request.Curl_Headers /= null then
         Curl_Slist_Free_All (Request.Curl_Headers);
         Request.Curl_Headers := null;
      end if;
      Interfaces.C.Strings.Free (Request.URL);
      Interfaces.C.Strings.Free (Request.Content);
   end Finalize;

end Util.Http.Clients.Curl;
