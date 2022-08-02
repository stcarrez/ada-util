------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2018, 2020, 2021, AdaCore                     --
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

with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Translator;
with AWS.Client.HTTP_Utils;
package body AWS.Client.Ext is

   procedure Do_Options
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List) is
   begin
      Send_Request
        (Connection, OPTIONS, Result, URI, No_Content, Headers);
   end Do_Options;

   function Do_Options
     (URL        : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout;
      Headers    : Header_List     := Empty_Header_List;
      User_Agent : String          := Default.User_Agent) return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts,
              User_Agent => User_Agent);

      Do_Options (Connection, Result, Headers => Headers);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Do_Options;

   procedure Do_Patch
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      URI        : String      := No_Data;
      Data       : String;
      Headers    : Header_List := Empty_Header_List) is
   begin
      Send_Request
        (Connection, PATCH, Result, URI, Translator.To_Stream_Element_Array (Data), Headers);
   end Do_Patch;

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
      User_Agent : String          := Default.User_Agent) return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts,
              User_Agent => User_Agent);

      Do_Patch (Connection, Result, Data => Data, Headers => Headers);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Do_Patch;

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
      User_Agent : String          := Default.User_Agent) return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts,
              User_Agent => User_Agent);

      Do_Delete (Connection, Result, Data, Headers => Headers);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Do_Delete;

   procedure Do_Delete
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Data       : String;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List) is
   begin
      Send_Request
        (Connection, DELETE, Result, URI, Translator.To_Stream_Element_Array (Data), Headers);
   end Do_Delete;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Connection   : in out HTTP_Connection;
      Kind         : Method_Kind;
      Result       : out Response.Data;
      URI          : String;
      Data         : Stream_Element_Array := No_Content;
      Headers      : Header_List := Empty_Header_List)
   is
      use Ada.Real_Time;
      Stamp         : constant Time := Clock;
      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
   begin
      Retry : loop
         begin
            HTTP_Utils.Open_Send_Common_Header
              (Connection, Method_Kind'Image (Kind), URI, Headers);

            --  If there is some data to send

            if Data'Length > 0 then
               HTTP_Utils.Send_Header
                 (Connection.Socket.all,
                  Messages.Content_Length (Data'Length));

               Net.Buffered.New_Line (Connection.Socket.all);

               --  Send message body

               Net.Buffered.Write (Connection.Socket.all, Data);

            else
               Net.Buffered.New_Line (Connection.Socket.all);
            end if;

            HTTP_Utils.Get_Response
              (Connection, Result,
               Get_Body => Kind /= HEAD and then not Connection.Streaming);

            HTTP_Utils.Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;

            elsif  Kind /= HEAD and then Connection.Streaming then
               HTTP_Utils.Read_Body (Connection, Result, Store => False);
            end if;

         exception
            when E : Net.Socket_Error | HTTP_Utils.Connection_Error =>
               Error_Processing
                 (Connection, Try_Count, Result,
                  Method_Kind'Image (Kind), E, Stamp);

               exit Retry when not Response.Is_Empty (Result);
         end;
      end loop Retry;
   end Send_Request;

end AWS.Client.Ext;
