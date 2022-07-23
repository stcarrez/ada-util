-----------------------------------------------------------------------
--  util-http-clients-curl -- HTTP Clients with CURL
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

package body Util.Http.Mockups is

   -----------------
   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   --  ------------------------------
   overriding
   function Contains_Header (Message : in Mockup_Message;
                             Name    : in String) return Boolean is
   begin
      return Message.Headers.Contains (Name);
   end Contains_Header;

   --  ------------------------------
   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   --  ------------------------------
   overriding
   function Get_Header (Message : in Mockup_Message;
                        Name    : in String) return String is
      Pos : constant Util.Strings.Maps.Cursor := Message.Headers.Find (Name);
   begin
      if Util.Strings.Maps.Has_Element (Pos) then
         return Util.Strings.Maps.Element (Pos);
      else
         return "";
      end if;
   end Get_Header;

   --  ------------------------------
   --  Sets a message header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   --  ------------------------------
   overriding
   procedure Set_Header (Message  : in out Mockup_Message;
                         Name     : in String;
                         Value    : in String) is
   begin
      Message.Headers.Include (Name, Value);
   end Set_Header;

   --  ------------------------------
   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   --  ------------------------------
   overriding
   procedure Add_Header (Message : in out Mockup_Message;
                         Name    : in String;
                         Value   : in String) is

      Pos : constant Util.Strings.Maps.Cursor := Message.Headers.Find (Name);
   begin
      if Util.Strings.Maps.Has_Element (Pos) then
         declare
            Header : constant String := Util.Strings.Maps.Element (Pos);
         begin
            Message.Headers.Replace_Element (Pos, Header & ASCII.CR & ASCII.LF & Value);
         end;
      else
         Message.Headers.Include (Name, Value);
      end if;
   end Add_Header;

   --  ------------------------------
   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   --  ------------------------------
   overriding
   procedure Iterate_Headers (Message : in Mockup_Message;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is
      Iter : Util.Strings.Maps.Cursor := Message.Headers.First;
   begin
      while Util.Strings.Maps.Has_Element (Iter) loop
         Util.Strings.Maps.Query_Element (Iter, Process);
         Util.Strings.Maps.Next (Iter);
      end loop;
   end Iterate_Headers;

   --  -------------
   --  ------------------------------
   --  Get the response body as a string.
   --  ------------------------------
   overriding
   function Get_Body (Reply : in Mockup_Response) return String is
   begin
      return Util.Strings.Builders.To_Array (Reply.Content);
   end Get_Body;

   --  ------------------------------
   --  Get the response body as a string.
   --  ------------------------------
   overriding
   function Get_Body (Reply : in Mockup_Response) return Util.Blobs.Blob_Ref is
      use Ada.Streams;

      Result : constant Util.Blobs.Blob_Ref
        := Util.Blobs.Create_Blob (Size => Util.Strings.Builders.Length (Reply.Content));
      Offset : Stream_Element_Offset := 1;

      procedure Append (Content : in String) is
         C : Stream_Element_Array (1 .. Stream_Element_Offset (Content'Length));
         for C'Address use Content'Address;
      begin
         Result.Value.Data (Offset .. Offset + C'Length - 1) := C;
         Offset := Offset + C'Length;
      end Append;

      procedure Copy is new Util.Strings.Builders.Inline_Iterate (Append);
   begin
      Copy (Reply.Content);
      return Result;
   end Get_Body;

   --  ------------------------------
   --  Get the response status code.
   --  ------------------------------
   overriding
   function Get_Status (Reply : in Mockup_Response) return Natural is
   begin
      return Reply.Status;
   end Get_Status;

   --  ------------------------------
   --  Set the response status code.
   --  ------------------------------
   procedure Set_Status (Reply  : in out Mockup_Response;
                         Status : in Natural) is
   begin
      Reply.Status := Status;
   end Set_Status;

   --  ------------------------------
   --  Set the response body.
   --  ------------------------------
   procedure Set_Body (Reply   : in out Mockup_Response;
                       Content : in String) is
   begin
      Util.Strings.Builders.Clear (Reply.Content);
      Util.Strings.Builders.Append (Reply.Content, Content);
   end Set_Body;

   --  ------------------------------
   --  Append the content to the response body.
   --  ------------------------------
   procedure Append_Body (Reply   : in out Mockup_Response;
                          Content : in String) is
   begin
      Util.Strings.Builders.Append (Reply.Content, Content);
   end Append_Body;

end Util.Http.Mockups;
