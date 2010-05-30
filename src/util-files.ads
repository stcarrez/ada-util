-----------------------------------------------------------------------
--  Util.Files -- Various File Utility Packages
--  Copyright (C) 2001, 2002, 2003, 2009, 2010 Stephane Carrez
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

   --  Find the file in one of the search directories.  Each search directory
   --  is separated by ';' (yes, even on Unix).
   --  Returns the path to be used for reading the file.
   function Find_File_Path (Name  : String;
                            Paths : String) return String;

end Util.Files;
