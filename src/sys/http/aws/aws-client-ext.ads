------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2017, 2020, 2021, 2022, AdaCore   --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;
pragma License (GPL);

with AWS.Response;

--  This is the AWS.Client.HTTP_Utils package revisited
--  and simplified to add support for HTTP Options and Patch.
package AWS.Client.Ext is

   No_Data    : constant String := "";
   No_Content : constant Stream_Element_Array := (1 .. 0 => 0);

   procedure Do_Options
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List);

   function Do_Options
     (URL        : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout;
      Headers    : Header_List     := Empty_Header_List;
      User_Agent : String          := Default.User_Agent) return Response.Data;
   --  Send to the server URL a OPTIONS request with Data

   procedure Do_Patch
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      URI        : String      := No_Data;
      Data       : String;
      Headers    : Header_List := Empty_Header_List);

   function Do_Patch
     (URL        : String;
      Data       : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout;
      Headers    : Header_List     := Empty_Header_List;
      User_Agent : String          := Default.User_Agent) return Response.Data;
   --  Send to the server URL a PATCH request with Data

   procedure Do_Delete
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Data       : String;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List);

   function Do_Delete
     (URL        : String;
      Data       : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout;
      Headers    : Header_List     := Empty_Header_List;
      User_Agent : String          := Default.User_Agent) return Response.Data;
   --  Send to the server URL a DELETE request with Data
   --  Delete will retry one time if it fails.

   type Method_Kind is (GET, HEAD, POST, PUT, DELETE, OPTIONS, PATCH);

   procedure Send_Request
     (Connection   : in out HTTP_Connection;
      Kind         : Method_Kind;
      Result       : out Response.Data;
      URI          : String;
      Data         : Stream_Element_Array := No_Content;
      Headers      : Header_List := Empty_Header_List);
   --  Send to the server only a POST request with Data
   --  and common headers, using a Connection.

end AWS.Client.Ext;
