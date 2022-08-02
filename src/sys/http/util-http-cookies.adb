-----------------------------------------------------------------------
--  util-http-cookies -- HTTP Cookies
--  Copyright (C) 2011, 2012, 2015, 2022 Stephane Carrez
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
with Ada.Strings.Maps;

with Util.Strings;
with Util.Strings.Builders;
with Util.Strings.Builders.Transforms;
with Util.Dates.RFC7231;
package body Util.Http.Cookies is

   use Ada.Strings.Unbounded;
   use Ada.Strings;
   use type Maps.Character_Set;

   --  Get number of cookies present in the header value
   function Get_Cookie_Count (Header : in String) return Natural;

   --  Check that the value contains valid character
   --  Raises Invalid_Value if an invalid character is found
   procedure Check_Value (S : in String);

   --  Check that the value contains valid character
   --  Raises Invalid_Value if an invalid character is found
   procedure Check_Token (S : in String);

   Forbidden : constant Maps.Character_Set
     := Maps.To_Set (Span => (Low  => Character'Val (0),
                              High => Character'Val (31)))
     or Maps.To_Set (";")
     or Maps.To_Set (Span => (Low  => Character'Val (127),
                              High => Character'Val (Character'Pos (Character'Last))));

   --  Reserved characters (See draft-ietf-httpstate-cookie-21 and RFC2616], Section 2.2)
   Reserved : constant Maps.Character_Set
     := Maps.To_Set ("()<>@,;:\""/[]?={} ") or Forbidden;

   --  ------------------------------
   --  Check that the value contains valid character
   --  Raises Invalid_Value if an invalid character is found
   --  ------------------------------
   procedure Check_Value (S : in String) is
   begin
      for I in S'Range loop
         if Maps.Is_In (Element => S (I), Set => Forbidden) then
            raise Invalid_Value with "Invalid character used: '" & S (I) & "'";
         end if;
      end loop;
   end Check_Value;

   --  ------------------------------
   --  Check that the value contains valid character
   --  Raises Invalid_Value if an invalid character is found
   --  ------------------------------
   procedure Check_Token (S : in String) is
   begin
      for I in S'Range loop
         if Maps.Is_In (Element => S (I), Set => Reserved) then
            raise Invalid_Value with "Reserved character used: '" & S (I) & "'";
         end if;
      end loop;
   end Check_Token;

   --  ------------------------------
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
   --  ------------------------------
   function Create (Name  : in String;
                    Value : in String) return Cookie is
      Result : Cookie;
   begin
      Check_Value (Value);
      Check_Token (Name);

      Result.Name  := To_Unbounded_String (Name);
      Result.Value := To_Unbounded_String (Value);
      return Result;
   end Create;

   --  ------------------------------
   --  Returns the name of the cookie. The name cannot be changed after creation.
   --  ------------------------------
   function Get_Name (Object : in Cookie) return String is
   begin
      return To_String (Object.Name);
   end Get_Name;

   --  ------------------------------
   --  Returns the value of the cookie.
   --  ------------------------------
   function Get_Value (Object : in Cookie) return String is
   begin
      return To_String (Object.Value);
   end Get_Value;

   --  ------------------------------
   --  Assigns a new value to a cookie after the cookie is created.
   --  If you use a binary value, you may want to use BASE64 encoding.
   --
   --  With Version 0 cookies, values should not contain white space, brackets,
   --  parentheses, equals signs, commas, double quotes, slashes, question marks,
   --  at signs, colons, and semicolons. Empty values may not behave
   --  the same way on all browsers.
   --  ------------------------------
   procedure Set_Value (Object : in out Cookie;
                        Value  : in String) is
   begin
      Check_Value (Value);
      Object.Value := To_Unbounded_String (Value);
   end Set_Value;

   --  ------------------------------
   --  Returns the comment describing the purpose of this cookie,
   --  or null if the cookie has no comment.
   --  ------------------------------
   function Get_Comment (Object : in Cookie) return String is
   begin
      return To_String (Object.Comment);
   end Get_Comment;

   --  ------------------------------
   --  Specifies a comment that describes a cookie's purpose. The comment is useful if
   --  the browser presents the cookie to the user. Comments are not supported by
   --  Netscape Version 0 cookies.
   --  ------------------------------
   procedure Set_Comment (Object  : in out Cookie;
                          Comment : in String) is
   begin
      Check_Value (Comment);
      Object.Comment := To_Unbounded_String (Comment);
   end Set_Comment;

   --  ------------------------------
   --  Returns the domain name set for this cookie. The form of the domain name
   --  is set by RFC 2109.
   --  ------------------------------
   function Get_Domain (Object : in Cookie) return String is
   begin
      return To_String (Object.Domain);
   end Get_Domain;

   --  ------------------------------
   --  Specifies the domain within which this cookie should be presented.
   --
   --  The form of the domain name is specified by RFC 2109. A domain name begins with
   --  a dot (.foo.com) and means that the cookie is visible to servers in a specified
   --  Domain Name System (DNS) zone (for example, www.foo.com, but not a.b.foo.com).
   --  By default, cookies are only returned to the server that sent them.
   --  ------------------------------
   procedure Set_Domain (Object : in out Cookie;
                         Domain : in String) is
   begin
      Check_Value (Domain);
      Object.Domain := To_Unbounded_String (Domain);
   end Set_Domain;

   --  ------------------------------
   --  Returns the maximum age of the cookie, specified in seconds.
   --  By default, -1 indicating the cookie will persist until browser shutdown.
   --  ------------------------------
   function Get_Max_Age (Object : in Cookie) return Integer is
   begin
      return Object.Max_Age;
   end Get_Max_Age;

   --  ------------------------------
   --  Sets the maximum age of the cookie in seconds.
   --
   --  A positive value indicates that the cookie will expire after that many seconds
   --  have passed. Note that the value is the maximum age when the cookie will expire,
   --  not the cookie's current age.
   --
   --  A negative value means that the cookie is not stored persistently and will be
   --  deleted when the Web browser exits. A zero value causes the cookie to be deleted.
   --  ------------------------------
   procedure Set_Max_Age (Object  : in out Cookie;
                          Max_Age : in Integer) is
   begin
      Object.Max_Age := Max_Age;
   end Set_Max_Age;

   --  ------------------------------
   --  Returns the path on the server to which the browser returns this cookie.
   --  The cookie is visible to all subpaths on the server.
   --  ------------------------------
   function Get_Path (Object : in Cookie) return String is
   begin
      return To_String (Object.Path);
   end Get_Path;

   --  ------------------------------
   --  Specifies a path for the cookie to which the client should return the cookie.
   --
   --  The cookie is visible to all the pages in the directory you specify,
   --  and all the pages in that directory's subdirectories. A cookie's path
   --  must include the servlet that set the cookie, for example, /catalog,
   --  which makes the cookie visible to all directories on the server under /catalog.
   --
   --  Consult RFC 2109 (available on the Internet) for more information on setting
   --  path names for cookies.
   --  ------------------------------
   procedure Set_Path (Object : in out Cookie;
                       Path   : in String) is
   begin
      Check_Value (Path);
      Object.Path := To_Unbounded_String (Path);
   end Set_Path;

   --  ------------------------------
   --  Returns true if the browser is sending cookies only over a secure protocol,
   --  or false if the browser can send cookies using any protocol.
   --  ------------------------------
   function Is_Secure (Object : in Cookie) return Boolean is
   begin
      return Object.Secure;
   end Is_Secure;

   --  ------------------------------
   --  Indicates to the browser whether the cookie should only be sent using
   --  a secure protocol, such as HTTPS or SSL.
   --  ------------------------------
   procedure Set_Secure (Object : in out Cookie;
                         Secure : in Boolean) is
   begin
      Object.Secure := Secure;
   end Set_Secure;

   --  ------------------------------
   --  Returns the version of the protocol this cookie complies with.
   --  Version 1 complies with RFC 2109, and version 0 complies with the original
   --  cookie specification drafted by Netscape. Cookies provided by a browser use
   --  and identify the browser's cookie version.
   --  ------------------------------
   function Get_Version (Object : in Cookie) return Natural is
   begin
      return Object.Version;
   end Get_Version;

   --  ------------------------------
   --  Sets the version of the cookie protocol this cookie complies with.
   --  Version 0 complies with the original Netscape cookie specification.
   --  Version 1 complies with RFC 2109.
   --  ------------------------------
   procedure Set_Version (Object  : in out Cookie;
                          Version : in Natural) is
   begin
      Object.Version := Version;
   end Set_Version;

   --  ------------------------------
   --  Returns True if the cookie has the http-only-flag.
   --  ------------------------------
   function Is_Http_Only (Object : in Cookie) return Boolean is
   begin
      return Object.Http_Only;
   end Is_Http_Only;

   --  ------------------------------
   --  Sets the http-only-flag associated with the cookie.  When the http-only-flag is
   --  set, the cookie is only for http protocols and not exposed to Javascript API.
   --  ------------------------------
   procedure Set_Http_Only (Object    : in out Cookie;
                            Http_Only : in Boolean) is
   begin
      Object.Http_Only := Http_Only;
   end Set_Http_Only;

   --  ------------------------------
   --  Get the cookie definition
   --  ------------------------------
   function To_Http_Header (Object : in Cookie) return String is
      use Util.Strings.Builders;
      use Ada.Calendar;

      V      : Natural := Object.Version;

      procedure Append_Value (Into  : in out Util.Strings.Builders.Builder;
                              Name  : in String;
                              Value : in Unbounded_String);

      procedure Append_Value (Into  : in out Util.Strings.Builders.Builder;
                              Name  : in String;
                              Value : in Unbounded_String) is
         Item : constant String := To_String (Value);
      begin
         if Name'Length > 0 then
            if Item'Length = 0 then
               return;
            end if;
            Append (Into, Name);
         end if;
         if Item'Length > 2 and then Item (Item'First) = '"' and then Item (Item'Last) = '"' then
            V := 1;
            Append (Into, '"');
            Util.Strings.Builders.Transforms.Escape_Java_Script (Content => Item,
                                                                 Into    => Into);
            Append (Into, '"');
         else
            Append (Into, Item);
         end if;
      end Append_Value;

      Result : Util.Strings.Builders.Builder (Len => 256);
      Buf    : Util.Strings.Builders.Builder (Len => 256);

   begin
      Append (Result, To_String (Object.Name));
      Append (Result, '=');
      Append_Value (Result, "", Object.Value);
      Append_Value (Buf, "; Domain=", Object.Domain);
      Append_Value (Buf, "; Path=", Object.Path);

      if Length (Object.Comment) > 0 then
         V := 1;
      end if;

      if V = 1 then
         Append (Result, "; Version=1");
         Append_Value (Result, "; Comment=", Object.Comment);
         Append (Result, "; Max-Age=");
         Append (Result, Util.Strings.Image (Object.Max_Age));

      elsif Object.Max_Age >= 0 then
         Append (Result, "; Expires=");
         if Object.Max_Age = 0 then
            Append (Result, "Thu, 01-Jan-1970 00:00:01 GMT");
         else
            Util.Dates.RFC7231.Append_Date (Result,
                                            Ada.Calendar.Clock + Duration (Object.Max_Age));
         end if;
      end if;
      Append_Value (Result, "; Domain=", Object.Domain);
      Append_Value (Result, "; Path=", Object.Path);

      if Object.Secure then
         Append (Result, "; Secure");
      end if;
      if Object.Http_Only then
         Append (Result, "; HttpOnly");
      end if;

      return Util.Strings.Builders.To_Array (Result);
   end To_Http_Header;

   --  ------------------------------
   --  Get number of cookies present in the header value
   --  ------------------------------
   function Get_Cookie_Count (Header : in String) return Natural is
      Pos      : Natural := Header'First;
      Cnt      : Natural := 0;
      In_Value : Boolean := False;
   begin
      while Pos <= Header'Last loop
         declare
            C : constant Character := Header (Pos);
         begin
            if In_Value then
               if C = ';' or else C = ',' then
                  In_Value := False;
               end if;
            else
               if C = '=' then
                  Cnt := Cnt + 1;
                  In_Value := True;
               end if;
            end if;
         end;
         Pos := Pos + 1;
      end loop;
      return Cnt;
   end Get_Cookie_Count;

   --  ------------------------------
   --  Parse the header and return an array of cookies.
   --  ------------------------------
   function Get_Cookies (Header : in String) return Cookie_Array_Access is
      Cnt        : constant Natural := Get_Cookie_Count (Header);
      Result     : constant Cookie_Array_Access := new Cookie_Array (1 .. Cnt);
      Pos        : Natural := Header'First;
      Idx        : Positive := 1;
      Start_Pos  : Natural;
      End_Pos    : Natural;
      Is_Special : Boolean;
      C          : Character;
   begin
      while Pos < Header'Last loop
         --  Skip spaces
         while Pos < Header'Last loop
            C := Header (Pos);
            exit when C /= ' ' and then C /= ASCII.HT;
            Pos := Pos + 1;
         end loop;

         Start_Pos := Pos;
         Is_Special := C = '$';
         if Is_Special then
            Pos := Pos + 1;
         end if;

         --  Find the token end position
         while Pos < Header'Last loop
            C := Header (Pos);
            exit when Maps.Is_In (Element => C, Set => Reserved);
            Pos := Pos + 1;
         end loop;

         End_Pos := Pos - 1;
         exit when Start_Pos > End_Pos;
         Result (Idx).Name := To_Unbounded_String (Header (Start_Pos .. End_Pos));

         --  Skip spaces
         while Pos < Header'Last loop
            C := Header (Pos);
            exit when C /= ' ' and then C /= ASCII.HT;
            Pos := Pos + 1;
         end loop;

         if C = '=' then
            Pos := Pos + 1;

            --  Skip spaces
            while Pos <= Header'Last loop
               C := Header (Pos);
               exit when C /= ' ' and then C /= ASCII.HT;
               Pos := Pos + 1;
            end loop;

            Start_Pos := Pos;
            if C = ';' then
               null;
            elsif C = '"' then
               Pos := Pos + 1;
            else
               Start_Pos := Pos;
               while Pos <= Header'Last loop
                  C := Header (Pos);
                  exit when Maps.Is_In (Element => C, Set => Forbidden);
                  Pos := Pos + 1;
               end loop;
            end if;
            if Start_Pos < Pos then
               Result (Idx).Value := To_Unbounded_String (Header (Start_Pos .. Pos - 1));
            end if;
         end if;
         Idx := Idx + 1;
         exit when Idx > Cnt;
         while Pos <= Header'Last loop
            C := Header (Pos);
            exit when C /= ' ' and then C /= ASCII.HT;
            Pos := Pos + 1;
         end loop;
         if C = ';' then
            Pos := Pos + 1;
         end if;
      end loop;
      return Result;
   end Get_Cookies;

end Util.Http.Cookies;
