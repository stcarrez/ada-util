-----------------------------------------------------------------------
--  Util.Files -- Various File Utility Packages
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2011 Stephane Carrez
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
with Util.Strings.Maps;
package Util.Files is

   use Ada.Strings.Unbounded;

   --  Read a complete file into a string.
   --  The <b>Max_Size</b> parameter indicates the maximum size that is read.
   procedure Read_File (Path     : in String;
                        Into     : out Unbounded_String;
                        Max_Size : in Natural := 0);

   --  Save the string into a file creating the file if necessary
   procedure Write_File (Path    : in String;
                         Content : in String);

   --  Save the string into a file creating the file if necessary
   procedure Write_File (Path    : in String;
                         Content : in Unbounded_String);

   --  Iterate over the search directories defined in <b>Path</b> and execute
   --  <b>Process</b> with each directory until it returns <b>True</b> in <b>Done</b>
   --  or the last search directory is found.  Each search directory
   --  is separated by ';' (yes, even on Unix).
   procedure Iterate_Path (Path   : in String;
                           Process : not null access procedure (Dir  : in String;
                                                                Done : out Boolean));

   --  Find the file in one of the search directories.  Each search directory
   --  is separated by ';' (yes, even on Unix).
   --  Returns the path to be used for reading the file.
   function Find_File_Path (Name  : String;
                            Paths : String) return String;

   --  Find the files which match the pattern in the directories specified in the
   --  search path <b>Path</b>.  Each search directory is separated by ';'.
   --  File names are added to the string set in <b>Into</b>.
   procedure Find_Files_Path (Pattern : in String;
                              Path    : in String;
                              Into    : in out Util.Strings.Maps.Map);

   --  Returns the name of the external file with the specified directory
   --  and the name.  Unlike the Ada.Directories.Compose, the name can represent
   --  a relative path and thus include directory separators.
   function Compose (Directory : in String;
                     Name      : in String) return String;

   --  Compose an existing path by adding the specified name to each path component
   --  and return a new paths having only existing directories.  Each directory is
   --  separated by ';'.
   --  If the composed path exists, it is added to the result path.
   --  Example:
   --    paths = 'web;regtests'  name = 'info'
   --    result = 'web/info;regtests/info'
   --  Returns the composed path.
   function Compose_Path (Paths : in String;
                          Name  : in String) return String;

end Util.Files;
