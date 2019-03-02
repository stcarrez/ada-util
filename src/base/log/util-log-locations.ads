-----------------------------------------------------------------------
--  util-log-locations -- General purpose source file location
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2017 Stephane Carrez
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

package Util.Log.Locations is

   --  ------------------------------
   --  Source line information
   --  ------------------------------
   type File_Info (<>) is limited private;
   type File_Info_Access is access all File_Info;

   --  Create a <b>File_Info</b> record to identify the file whose path is <b>Path</b>
   --  and whose relative path portion starts at <b>Relative_Position</b>.
   function Create_File_Info (Path              : in String;
                              Relative_Position : in Natural) return File_Info_Access;

   --  Get the relative path name
   function Relative_Path (File : in File_Info) return String;

   type Line_Info is private;

   --  Get the line number
   function Line (Info : in Line_Info) return Natural;

   --  Get the column number
   function Column (Info : in Line_Info) return Natural;

   --  Get the source file
   function File (Info : in Line_Info) return String;

   --  Compare the two source location.  The comparison is made on:
   --  o the source file,
   --  o the line number,
   --  o the column.
   function "<" (Left, Right : in Line_Info) return Boolean;

   --  Create a source line information.
   function Create_Line_Info (File   : in File_Info_Access;
                              Line   : in Natural;
                              Column : in Natural := 0) return Line_Info;

   --  Get a printable representation of the line information using
   --  the format:
   --    <path>:<line>[:<column>]
   --  The path can be reported as relative or absolute path.
   --  The column is optional and reported by default.
   function To_String (Info     : in Line_Info;
                       Relative : in Boolean := True;
                       Column   : in Boolean := True) return String;

private
   pragma Inline (Line);
   pragma Inline (File);

   type File_Info (Length : Natural) is limited record
      Relative_Pos : Natural;
      Path         : String (1 .. Length);
   end record;

   NO_FILE : aliased File_Info := File_Info '(Length => 0, Path => "", Relative_Pos => 1);

   type Line_Info is record
      Line   : Natural := 0;
      Column : Natural := 0;
      File   : File_Info_Access := NO_FILE'Access;
   end record;

end Util.Log.Locations;
