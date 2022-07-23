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

with Ada.Strings.Unbounded;
with Util.Http.Clients.AWS_I.Messages;
with Util.Http.Clients.AWS_I.Headers.Set;
with Util.Log.Loggers;
with AWS.Client.Ext;
package body Util.Http.Clients.AWS is

   use type AWS_I.Messages.Status_Code;

   Log   : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Http.Clients.AWS");

   Manager : aliased AWS_Http_Manager;

   --  ------------------------------
   --  Register the Http manager.
   --  ------------------------------
   procedure Register is
   begin
      Default_Http_Manager := Manager'Access;
   end Register;

   function To_Status (Code : in AWS_I.Messages.Status_Code) return Natural;

   procedure Set_Result (Into : in AWS_Http_Response_Access;
                         Data : in AWS_I.Response.Data);

   function To_Status (Code : in AWS_I.Messages.Status_Code) return Natural is
      use AWS_I.Messages;
   begin
      case Code is
         when S100 =>
            return 100;
         when S101 =>
            return 101;
         when S102 =>
            return 102;
         when S200 =>
            return 200;
         when S201 =>
            return 201;
         when S203 =>
            return 203;
         when S204 =>
            return 204;
         when S205 =>
            return 205;
         when S206 =>
            return 206;
         when S207 =>
            return 207;
         when S300 =>
            return 300;
         when S301 =>
            return 301;
         when S302 =>
            return 302;
         when S303 =>
            return 303;
         when S304 =>
            return 304;
         when S305 =>
            return 305;
         when S307 =>
            return 307;
         when S400 =>
            return 400;
         when S401 =>
            return 401;
         when S402 =>
            return 402;
         when S403 =>
            return 403;
         when S404 =>
            return 404;
         when S405 =>
            return 405;
         when S406 =>
            return 406;
         when S407 =>
            return 407;
         when S408 =>
            return 408;
         when S409 =>
            return 409;
         when S410 =>
            return 410;
         when S411 =>
            return 411;
         when S412 =>
            return 412;
         when S413 =>
            return 413;
         when S414 =>
            return 414;
         when S415 =>
            return 415;
         when S416 =>
            return 416;
         when S417 =>
            return 417;
         when S422 =>
            return 422;
         when S423 =>
            return 423;
         when S424 =>
            return 424;
         when S500 =>
            return 500;
         when S501 =>
            return 501;
         when S502 =>
            return 502;
         when S503 =>
            return 503;
         when S504 =>
            return 504;
         when S505 =>
            return 505;
         when S507 =>
            return 507;
         when others =>
            return 500;
      end case;
   end To_Status;

   overriding
   procedure Create (Manager  : in AWS_Http_Manager;
                     Http     : in out Client'Class) is
      pragma Unreferenced (Manager);
   begin
      Http.Delegate := new AWS_Http_Request;
   end Create;

   procedure Set_Result (Into : in AWS_Http_Response_Access;
                         Data : in AWS_I.Response.Data) is
   begin
      Into.Data := Data;
      if AWS_I.Response.Status_Code (Data) = AWS_I.Messages.S408 then
         raise Connection_Error with AWS_I.Response.Message_Body (Data);
      end if;
   end Set_Result;

   overriding
   procedure Do_Get (Manager  : in AWS_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class) is
      pragma Unreferenced (Manager);

      Req     : constant AWS_Http_Request_Access
        := AWS_Http_Request'Class (Http.Delegate.all)'Access;
      Rep     : constant AWS_Http_Response_Access := new AWS_Http_Response;
   begin
      Log.Info ("Get {0}", URI);

      Reply.Delegate := Rep.all'Access;
      Set_Result (Rep, AWS_I.Client.Get (URL => URI, Headers => Req.Headers,
                                         Timeouts => Req.Timeouts));
   end Do_Get;

   overriding
   procedure Do_Head (Manager  : in AWS_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Reply    : out Response'Class) is
      pragma Unreferenced (Manager);

      Req     : constant AWS_Http_Request_Access
        := AWS_Http_Request'Class (Http.Delegate.all)'Access;
      Rep     : constant AWS_Http_Response_Access := new AWS_Http_Response;
   begin
      Log.Info ("Head {0}", URI);

      Reply.Delegate := Rep.all'Access;
      Set_Result (Rep, AWS_I.Client.Head (URL => URI, Headers => Req.Headers,
                                          Timeouts => Req.Timeouts));
   end Do_Head;

   overriding
   procedure Do_Post (Manager  : in AWS_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class) is
      pragma Unreferenced (Manager);

      Req     : constant AWS_Http_Request_Access
        := AWS_Http_Request'Class (Http.Delegate.all)'Access;
      Rep     : constant AWS_Http_Response_Access := new AWS_Http_Response;
   begin
      Log.Info ("Post {0}", URI);

      Reply.Delegate := Rep.all'Access;
      Set_Result (Rep, AWS_I.Client.Post (URL => URI, Data => Data,
                                          Headers => Req.Headers,
                                          Timeouts => Req.Timeouts));
   end Do_Post;

   overriding
   procedure Do_Put (Manager  : in AWS_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Data     : in String;
                     Reply    : out Response'Class) is
      pragma Unreferenced (Manager);

      Req     : constant AWS_Http_Request_Access
        := AWS_Http_Request'Class (Http.Delegate.all)'Access;
      Rep     : constant AWS_Http_Response_Access := new AWS_Http_Response;
   begin
      Log.Info ("Put {0}", URI);

      Reply.Delegate := Rep.all'Access;
      Set_Result (Rep, AWS_I.Client.Put (URL => URI, Data => Data, Headers => Req.Headers,
                                         Timeouts => Req.Timeouts));
   end Do_Put;

   overriding
   procedure Do_Patch (Manager  : in AWS_Http_Manager;
                       Http     : in Client'Class;
                       URI      : in String;
                       Data     : in String;
                       Reply    : out Response'Class) is
      pragma Unreferenced (Manager);

      Req     : constant AWS_Http_Request_Access
        := AWS_Http_Request'Class (Http.Delegate.all)'Access;
      Rep     : constant AWS_Http_Response_Access := new AWS_Http_Response;
   begin
      Log.Info ("Patch {0}", URI);

      Reply.Delegate := Rep.all'Access;
      Set_Result (Rep, AWS_I.Client.Ext.Do_Patch (URL => URI, Data => Data, Headers => Req.Headers,
                                                  Timeouts => Req.Timeouts));
   end Do_Patch;

   overriding
   procedure Do_Options (Manager  : in AWS_Http_Manager;
                         Http     : in Client'Class;
                         URI      : in String;
                         Reply    : out Response'Class) is
      pragma Unreferenced (Manager);

      Req     : constant AWS_Http_Request_Access
        := AWS_Http_Request'Class (Http.Delegate.all)'Access;
      Rep     : constant AWS_Http_Response_Access := new AWS_Http_Response;
   begin
      Log.Info ("Options {0}", URI);

      Reply.Delegate := Rep.all'Access;
      Set_Result (Rep, AWS_I.Client.Ext.Do_Options (URL => URI, Headers => Req.Headers,
                                                    Timeouts => Req.Timeouts));
   end Do_Options;

   overriding
   procedure Do_Delete (Manager  : in AWS_Http_Manager;
                        Http     : in Client'Class;
                        URI      : in String;
                        Reply    : out Response'Class) is
      pragma Unreferenced (Manager);
      Req     : constant AWS_Http_Request_Access
        := AWS_Http_Request'Class (Http.Delegate.all)'Access;
      Rep     : constant AWS_Http_Response_Access := new AWS_Http_Response;
   begin
      Log.Info ("Delete {0}", URI);

      Reply.Delegate := Rep.all'Access;
      Set_Result (Rep, AWS_I.Client.Ext.Do_Delete (URL => URI, Data => "", Headers => Req.Headers,
                                                   Timeouts => Req.Timeouts));
   end Do_Delete;

   --  ------------------------------
   --  Set the timeout for the connection.
   --  ------------------------------
   overriding
   procedure Set_Timeout (Manager : in AWS_Http_Manager;
                          Http    : in Client'Class;
                          Timeout : in Duration) is
      pragma Unreferenced (Manager);
   begin
      AWS_Http_Request'Class (Http.Delegate.all).Timeouts
         := AWS_I.Client.Timeouts (Connect  => Timeout,
                                 Send     => Timeout,
                                 Receive  => Timeout,
                                 Response => Timeout);
   end Set_Timeout;

   --  ------------------------------
   --  Returns a boolean indicating whether the named request header has already
   --  been set.
   --  ------------------------------
   overriding
   function Contains_Header (Http : in AWS_Http_Request;
                             Name : in String) return Boolean is
      Values : constant AWS_I.Headers.VString_Array
        := AWS_I.Headers.Get_Values (Http.Headers, Name);
   begin
      return Values'Length > 0;
   end Contains_Header;

   --  ------------------------------
   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   --  ------------------------------
   overriding
   function Get_Header (Request : in AWS_Http_Request;
                        Name    : in String) return String is
      Values : constant AWS_I.Headers.VString_Array
        := AWS_I.Headers.Get_Values (Request.Headers, Name);
   begin
      if Values'Length > 0 then
         return Ada.Strings.Unbounded.To_String (Values (Values'First));
      else
         return "";
      end if;
   end Get_Header;

   --  ------------------------------
   --  Sets a request header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   --  ------------------------------
   overriding
   procedure Set_Header (Http  : in out AWS_Http_Request;
                         Name  : in String;
                         Value : in String) is
   begin
      AWS_I.Headers.Set.Add (Http.Headers, Name, Value);
   end Set_Header;

   --  ------------------------------
   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   --  ------------------------------
   overriding
   procedure Add_Header (Http  : in out AWS_Http_Request;
                         Name  : in String;
                         Value : in String) is
   begin
      AWS_I.Headers.Set.Add (Http.Headers, Name, Value);
   end Add_Header;

   --  Iterate over the request headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Request : in AWS_Http_Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is
   begin
      null;
   end Iterate_Headers;

   --  ------------------------------
   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   --  ------------------------------
   overriding
   function Contains_Header (Reply : in AWS_Http_Response;
                             Name  : in String) return Boolean is
   begin
      return AWS_I.Response.Header (Reply.Data, Name) /= "";
   end Contains_Header;

   --  ------------------------------
   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   --  ------------------------------
   overriding
   function Get_Header (Reply  : in AWS_Http_Response;
                        Name   : in String) return String is
   begin
      return AWS_I.Response.Header (Reply.Data, Name);
   end Get_Header;

   --  Sets a message header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Reply    : in out AWS_Http_Response;
                         Name     : in String;
                         Value    : in String) is
   begin
      null;
   end Set_Header;

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   overriding
   procedure Add_Header (Reply   : in out AWS_Http_Response;
                         Name    : in String;
                         Value   : in String) is
   begin
      null;
   end Add_Header;

   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Reply   : in AWS_Http_Response;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is
   begin
      null;
   end Iterate_Headers;

   --  ------------------------------
   --  Get the response body as a string.
   --  ------------------------------
   overriding
   function Get_Body (Reply : in AWS_Http_Response) return String is
   begin
      return AWS_I.Response.Message_Body (Reply.Data);
   end Get_Body;

   overriding
   function Get_Body (Reply : in AWS_Http_Response) return Util.Blobs.Blob_Ref is
      Data : constant Ada.Streams.Stream_Element_Array
        := AWS_I.Response.Message_Body (Reply.Data);
   begin
      return Util.Blobs.Create_Blob (Data);
   end Get_Body;

   --  Get the response status code.
   overriding
   function Get_Status (Reply : in AWS_Http_Response) return Natural is
   begin
      return To_Status (AWS_I.Response.Status_Code (Reply.Data));
   end Get_Status;

end Util.Http.Clients.AWS;
