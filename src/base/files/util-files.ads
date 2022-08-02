-----------------------------------------------------------------------
--  util-files -- Various File Utility Packages
--  Copyright (C) 2001 - 2022 Stephane Carrez
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
with Util.Strings.Vectors;

--  = Files =
--  The `Util.Files` package provides various utility operations around files
--  to help in reading, writing, searching for files in a path.
--  To use the operations described here, use the following GNAT project:
--
--    with "utilada_base";
--
--  == Reading and writing ==
--  To easily get the full content of a file, the `Read_File` procedure can be
--  used.  A first form exists that populates a `Unbounded_String` or a vector
--  of strings.  A second form exists with a procedure that is called with each
--  line while the file is read.  These different forms simplify the reading of
--  files as it is possible to write:
--
--    Content : Ada.Strings.Unbounded.Unbounded_String;
--    Util.Files.Read_File ("config.txt", Content);
--
--  or
--
--    List : Util.Strings.Vectors.Vector;
--    Util.Files.Read_File ("config.txt", List);
--
--  or
--
--    procedure Read_Line (Line : in String) is ...
--    Util.Files.Read_File ("config.txt", Read_Line'Access);
--
--  Similarly, writing a file when you have a string or an `Unbounded_String`
--  is easily written by using `Write_File` as follows:
--
--    Util.Files.Write_File ("config.txt", "full content");
--
--  == Searching files ==
--  Searching for a file in a list of directories can be accomplished by using
--  the `Iterate_Path`, `Iterate_Files_Path` or `Find_File_Path`.
--
--  The `Find_File_Path` function is helpful to find a file in some `PATH`
--  search list.  The function looks in each search directory for the given
--  file name and it builds and returns the computed path of the first file
--  found in the search list.  For example:
--
--    Path : String := Util.Files.Find_File_Path ("ls",
--                                                "/bin:/usr/bin",
--                                                ':');
--
--  This will return `/usr/bin/ls` on most Unix systems.
--
--  @include util-files-rolling.ads
package Util.Files is

   use Ada.Strings.Unbounded;

   subtype Direction is Ada.Strings.Direction;

   --  Read a complete file into a string.
   --  The <b>Max_Size</b> parameter indicates the maximum size that is read.
   procedure Read_File (Path     : in String;
                        Into     : out Unbounded_String;
                        Max_Size : in Natural := 0);

   --  Read the file with the given path, one line at a time and execute the <b>Process</b>
   --  procedure with each line as argument.
   procedure Read_File (Path     : in String;
                        Process  : not null access procedure (Line : in String));

   --  Read the file with the given path, one line at a time and append each line to
   --  the <b>Into</b> vector.
   procedure Read_File (Path  : in String;
                        Into  : in out Util.Strings.Vectors.Vector);

   --  Save the string into a file creating the file if necessary
   procedure Write_File (Path    : in String;
                         Content : in String);

   --  Save the string into a file creating the file if necessary
   procedure Write_File (Path    : in String;
                         Content : in Unbounded_String);

   --  Iterate over the search directories defined in <b>Path</b> and execute
   --  <b>Process</b> with each directory until it returns <b>True</b> in <b>Done</b>
   --  or the last search directory is found.  Each search directory
   --  is separated by ';' (yes, even on Unix).  When <b>Going</b> is set to Backward, the
   --  directories are searched in reverse order.
   procedure Iterate_Path (Path     : in String;
                           Process  : not null access procedure (Dir  : in String;
                                                                 Done : out Boolean);
                           Going    : in Direction := Ada.Strings.Forward);

   --  Iterate over the search directories defined in <b>Path</b> and search
   --  for files matching the pattern defined by <b>Pattern</b>.  For each file,
   --  execute <b>Process</b> with the file basename and the full file path.
   --  Stop iterating when the <b>Process</b> procedure returns True.
   --  Each search directory is separated by ';'.  When <b>Going</b> is set to Backward, the
   --  directories are searched in reverse order.
   procedure Iterate_Files_Path (Pattern  : in String;
                                 Path     : in String;
                                 Process  : not null access procedure (Name : in String;
                                                                       File : in String;
                                                                       Done : out Boolean);
                                 Going    : in Direction := Ada.Strings.Forward);

   --  Find the file `Name` in one of the search directories defined in `Paths`.
   --  Each search directory is separated by ';' by default (yes, even on Unix).
   --  This can be changed by specifying the `Separator` value.
   --  Returns the path to be used for reading the file.
   function Find_File_Path (Name      : in String;
                            Paths     : in String;
                            Separator : in Character := ';') return String;

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
   --  separated by ';' (this can be overriding with the `Separator` parameter).
   --  If the composed path exists, it is added to the result path.
   --  Example:
   --    paths = 'web;regtests'  name = 'info'
   --    result = 'web/info;regtests/info'
   --  Returns the composed path.
   function Compose_Path (Paths     : in String;
                          Name      : in String;
                          Separator : in Character := ';') return String;

   --  Returns a relative path whose origin is defined by <b>From</b> and which refers
   --  to the absolute path referenced by <b>To</b>.  Both <b>From</b> and <b>To</b> are
   --  assumed to be absolute paths.  Returns the absolute path <b>To</b> if the relative
   --  path could not be found.  Both paths must have at least one root component in common.
   function Get_Relative_Path (From : in String;
                               To   : in String) return String;

   --  Rename the old name into a new name.
   procedure Rename (Old_Name, New_Name : in String);

   --  Delete the file including missing symbolic link
   --  or socket files (which GNAT fails to delete,
   --  see gcc/63222 and gcc/56055).  The function returns 0
   --  or the system error code.  The procedure raises the Use_Error
   --  exception if the file cannot be deleted.
   function Delete_File (Path : in String) return Integer;
   procedure Delete_File (Path : in String);

   --  Delete the directory tree recursively.  If the directory tree contains
   --  sockets, special files and dangling symbolic links, they are removed
   --  correctly.  This is a workaround for GNAT bug gcc/63222 and gcc/56055.
   procedure Delete_Tree (Path : in String);

   --  Find the canonicalized absolute path of the given file.
   function Realpath (Path : in String) return String;

end Util.Files;
