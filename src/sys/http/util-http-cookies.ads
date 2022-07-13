-----------------------------------------------------------------------
--  util-http-cookies -- HTTP Cookies
--  Copyright (C) 2011, 2012, 2022 Stephane Carrez
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

package Util.Http.Cookies is

   --  Exception raised if the value contains an invalid character.
   Invalid_Value : exception;

   type Cookie is private;

   type Cookie_Array is array (Natural range <>) of Cookie;

   type Cookie_Array_Access is access all Cookie_Array;

   --  Constructs a cookie with a specified name and value.
   --
   --  The name must conform to RFC 2109. That means it can contain only ASCII alphanumeric
   --  characters and cannot contain commas, semicolons, or white space or begin with
   --  a $ character. The cookie's name cannot be changed after creation.
   --
   --  The value can be anything the server chooses to send. Its value is probably
   --  of interest only to the server. The cookie's value can be changed after creation
   --  with the setValue method.
   --
   --  By default, cookies are created according to the Netscape cookie specification.
   --  The version can be changed with the setVersion method.
   function Create (Name  : in String;
                    Value : in String) return Cookie;

   --  Returns the name of the cookie. The name cannot be changed after creation.
   function Get_Name (Object : in Cookie) return String;

   --  Returns the value of the cookie.
   function Get_Value (Object : in Cookie) return String;

   --  Assigns a new value to a cookie after the cookie is created.
   --  If you use a binary value, you may want to use BASE64 encoding.
   --
   --  With Version 0 cookies, values should not contain white space, brackets,
   --  parentheses, equals signs, commas, double quotes, slashes, question marks,
   --  at signs, colons, and semicolons. Empty values may not behave
   --  the same way on all browsers.
   procedure Set_Value (Object : in out Cookie;
                        Value  : in String);
   --  Returns the comment describing the purpose of this cookie,
   --  or null if the cookie has no comment.
   function Get_Comment (Object : in Cookie) return String;

   --  Specifies a comment that describes a cookie's purpose. The comment is useful if
   --  the browser presents the cookie to the user. Comments are not supported by
   --  Netscape Version 0 cookies.
   procedure Set_Comment (Object  : in out Cookie;
                          Comment : in String);

   --  Returns the domain name set for this cookie. The form of the domain name
   --  is set by RFC 2109.
   function Get_Domain (Object : in Cookie) return String;

   --  Specifies the domain within which this cookie should be presented.
   --
   --  The form of the domain name is specified by RFC 2109. A domain name begins with
   --  a dot (.foo.com) and means that the cookie is visible to servers in a specified
   --  Domain Name System (DNS) zone (for example, www.foo.com, but not a.b.foo.com).
   --  By default, cookies are only returned to the server that sent them.
   procedure Set_Domain (Object : in out Cookie;
                         Domain : in String);

   --  Returns the maximum age of the cookie, specified in seconds.
   --  By default, -1 indicating the cookie will persist until browser shutdown.
   function Get_Max_Age (Object : in Cookie) return Integer;

   --  Sets the maximum age of the cookie in seconds.
   --
   --  A positive value indicates that the cookie will expire after that many seconds
   --  have passed. Note that the value is the maximum age when the cookie will expire,
   --  not the cookie's current age.
   --
   --  A negative value means that the cookie is not stored persistently and will be
   --  deleted when the Web browser exits. A zero value causes the cookie to be deleted.
   procedure Set_Max_Age (Object  : in out Cookie;
                          Max_Age : in Integer);

   --  Returns the path on the server to which the browser returns this cookie.
   --  The cookie is visible to all subpaths on the server.
   function Get_Path (Object : in Cookie) return String;

   --  Specifies a path for the cookie to which the client should return the cookie.
   --
   --  The cookie is visible to all the pages in the directory you specify,
   --  and all the pages in that directory's subdirectories. A cookie's path
   --  must include the servlet that set the cookie, for example, /catalog,
   --  which makes the cookie visible to all directories on the server under /catalog.
   --
   --  Consult RFC 2109 (available on the Internet) for more information on setting
   --  path names for cookies.
   procedure Set_Path (Object : in out Cookie;
                       Path   : in String);

   --  Returns true if the browser is sending cookies only over a secure protocol,
   --  or false if the browser can send cookies using any protocol.
   function Is_Secure (Object : in Cookie) return Boolean;

   --  Indicates to the browser whether the cookie should only be sent using
   --  a secure protocol, such as HTTPS or SSL.
   procedure Set_Secure (Object : in out Cookie;
                         Secure : in Boolean);

   --  Returns the version of the protocol this cookie complies with.
   --  Version 1 complies with RFC 2109, and version 0 complies with the original
   --  cookie specification drafted by Netscape. Cookies provided by a browser use
   --  and identify the browser's cookie version.
   function Get_Version (Object : in Cookie) return Natural;

   --  Sets the version of the cookie protocol this cookie complies with.
   --  Version 0 complies with the original Netscape cookie specification.
   --  Version 1 complies with RFC 2109.
   procedure Set_Version (Object  : in out Cookie;
                          Version : in Natural);

   --  Returns True if the cookie has the http-only-flag.
   function Is_Http_Only (Object : in Cookie) return Boolean;

   --  Sets the http-only-flag associated with the cookie.  When the http-only-flag is
   --  set, the cookie is only for http protocols and not exposed to Javascript API.
   procedure Set_Http_Only (Object    : in out Cookie;
                            Http_Only : in Boolean);

   --  Get the cookie definition
   function To_Http_Header (Object : in Cookie) return String;

   --  Parse the header and return an array of cookies.
   function Get_Cookies (Header : in String) return Cookie_Array_Access;

private

   type Cookie is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Value     : Ada.Strings.Unbounded.Unbounded_String;
      Domain    : Ada.Strings.Unbounded.Unbounded_String;
      Path      : Ada.Strings.Unbounded.Unbounded_String;
      Comment   : Ada.Strings.Unbounded.Unbounded_String;
      Max_Age   : Integer := -1;
      Secure    : Boolean := False;
      Http_Only : Boolean := False;
      Version   : Natural := 0;
   end record;

end Util.Http.Cookies;
