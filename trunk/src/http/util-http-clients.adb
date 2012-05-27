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

with Ada.Unchecked_Deallocation;
package body Util.Http.Clients is

   --  ------------------------------
   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   --  ------------------------------
   function Contains_Header (Reply : in Response;
                             Name  : in String) return Boolean is
   begin
      return Reply.Data.Contains_Header (Name);
   end Contains_Header;

   --  ------------------------------
   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   --  ------------------------------
   function Get_Header (Reply  : in Response;
                        Name   : in String) return String is
   begin
      return Reply.Data.Get_Header (Name);
   end Get_Header;

   --  ------------------------------
   --  Get the response body as a string.
   --  ------------------------------
   function Get_Body (Reply : in Response) return String is
   begin
      return Reply.Data.Get_Body;
   end Get_Body;

   --  ------------------------------
   --  Get the response status code.
   --  ------------------------------
   function Get_Status (Reply : in Response) return Natural is
   begin
      return Reply.Status;
   end Get_Status;

   --  Sets a header with the given name and date-value.
   --  The date is specified in terms of milliseconds since the epoch.
   --  If the header had already been set, the new value overwrites the previous one.
   --  The containsHeader method can be used to test for the presence of a header
   --  before setting its value.
   procedure Set_Date_Header (Request  : in out Client;
                              Name     : in String;
                              Date     : in Ada.Calendar.Time) is
   begin
      null;
   end Set_Date_Header;

   --  Adds a header with the given name and date-value. The date is specified
   --  in terms of milliseconds since the epoch. This method allows response headers
   --  to have multiple values.
   procedure Add_Date_Header (Request : in out Client;
                              Name    : in String;
                              Date    : in Ada.Calendar.Time) is
   begin
      null;
   end Add_Date_Header;

   --  Sets a header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
--     procedure Set_Header (Request  : in out Client;
--                           Name     : in String;
--                           Value    : in String) is
--     begin
--        null;
--     end Set_Header;

   --  Adds a header with the given name and value.
   --  This method allows headers to have multiple values.
--     procedure Add_Header (Request  : in out Client;
--                           Name     : in String;
--                           Value    : in String) is
--     begin
--        null;
--     end Add_Header;

   --  Sets a header with the given name and integer value.
   --  If the header had already been set, the new value overwrites the previous one.
   --  The containsHeader  method can be used to test for the presence of a header
   --  before setting its value.
   procedure Set_Int_Header (Request  : in out Client;
                             Name     : in String;
                             Value    : in Integer) is
   begin
      null;
   end Set_Int_Header;

   --  Adds a header with the given name and integer value. This method
   --  allows headers to have multiple values.
   procedure Add_Int_Header (Request  : in out Client;
                             Name     : in String;
                             Value    : in Integer) is
   begin
      null;
   end Add_Int_Header;

   --  Removes all headers with the given name.
   procedure Remove_Header (Request : in out Client;
                            Name    : in String) is
   begin
      null;
   end Remove_Header;

   --  Returns a boolean indicating whether the named header has already been set.
--     function Contains_Header (Request : in Client;
--                               Name    : in String) return Boolean is
--     begin
--        return False;
--     end Contains_Header;

   --  ------------------------------
   --  Initialize the client
   --  ------------------------------
   overriding
   procedure Initialize (Http : in out Client) is
   begin
      Http.Request  := null;
      Http.Manager  := Default_Http_Manager;
      Http.Manager.Create (Http);
   end Initialize;

   overriding
   procedure Finalize (Http : in out Client) is
      procedure Free is new Ada.Unchecked_Deallocation (Http_Request'Class,
                                                        Http_Request_Access);
   begin
      Free (Http.Request);
   end Finalize;

   --  ------------------------------
   --  Execute an http GET request on the given URL.  Additional request parameters,
   --  cookies and headers should have been set on the client object.
   --  ------------------------------
   procedure Get (Request  : in out Client;
                  URL      : in String;
                  Reply    : out Response'Class) is
   begin
      Request.Manager.Do_Get (Request, URL, Reply);
   end Get;

   --  ------------------------------
   --  Execute an http POST request on the given URL.  Additional request parameters,
   --  cookies and headers should have been set on the client object.
   --  ------------------------------
   procedure Post (request  : in out Client;
                   URL      : in String;
                   Data     : in String;
                   Reply    : out Response'Class) is
   begin
      Request.Manager.Do_Post (Request, URL, Data, Reply);
   end Post;

   --  ------------------------------
   --  Adds the specified cookie to the request. This method can be called multiple
   --  times to set more than one cookie.
   --  ------------------------------
   procedure Add_Cookie (Http   : in out Client;
                         Cookie : in Util.Http.Cookies.Cookie) is
   begin
      null;
   end Add_Cookie;

   --  ------------------------------
   --  Returns a boolean indicating whether the named request header has already
   --  been set.
   --  ------------------------------
   function Contains_Header (Request : in Client;
                             Name    : in String) return Boolean is
   begin
      return Request.Request.Contains_Header (Name);
   end Contains_Header;

   --  ------------------------------
   --  Sets a request header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   --  ------------------------------
   procedure Set_Header (Request : in out Client;
                         Name    : in String;
                         Value   : in String) is
   begin
      Request.Request.Set_Header (Name, Value);
   end Set_Header;

   --  ------------------------------
   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   --  ------------------------------
   procedure Add_Header (Request : in out Client;
                         Name    : in String;
                         Value   : in String) is
   begin
      Request.Request.Add_Header (Name, Value);
   end Add_Header;

   --  ------------------------------
   --  Free the resource used by the response.
   --  ------------------------------
   overriding
   procedure Finalize (Reply : in out Response) is
      procedure Free is new Ada.Unchecked_Deallocation (Http_Response'Class,
                                                        Http_Response_Access);
   begin
      Free (Reply.Data);
   end Finalize;

end Util.Http.Clients;
