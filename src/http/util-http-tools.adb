-----------------------------------------------------------------------
--  util-http-tools -- HTTP Utility Library
--  Copyright (C) 2012 Stephane Carrez
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
