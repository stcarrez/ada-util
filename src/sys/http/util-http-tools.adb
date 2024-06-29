-----------------------------------------------------------------------
--  util-http-tools -- HTTP Utility Library
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Util.Files;
package body Util.Http.Tools is

   --  ------------------------------
   --  Save the content stored in the HTTP response into a file.
   --  The response headers are saved only when <b>Save_Headers</b> is true.
   --  ------------------------------
   procedure Save_Response (Path         : in String;
                            Response     : in Abstract_Response'Class;
                            Save_Headers : in Boolean := False) is
      procedure Write_Header (Name  : in String;
                              Value : in String);

      Data : Ada.Strings.Unbounded.Unbounded_String;

      procedure Write_Header (Name  : in String;
                              Value : in String) is
      begin
         Ada.Strings.Unbounded.Append (Data, Name);
         Ada.Strings.Unbounded.Append (Data, ": ");
         Ada.Strings.Unbounded.Append (Data, Value);
         Ada.Strings.Unbounded.Append (Data, ASCII.CR & ASCII.LF);
      end Write_Header;

   begin
      if Save_Headers then
         Response.Iterate_Headers (Write_Header'Access);
      end if;
      Ada.Strings.Unbounded.Append (Data, Response.Get_Body);
      Util.Files.Write_File (Path    => Path,
                             Content => Ada.Strings.Unbounded.To_String (Data));
   end Save_Response;

end Util.Http.Tools;
