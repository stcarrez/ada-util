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
with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Util.Strings.Builders;
with Util.Strings.Tokenizers;
with Util.Systems.Os;
with Util.Systems.Types;
package body Util.Files is

   --  ------------------------------
   --  Read a complete file into a string.
   --  The <b>Max_Size</b> parameter indicates the maximum size that is read.
   --  ------------------------------
   procedure Read_File (Path     : in String;
                        Into     : out Unbounded_String;
                        Max_Size : in Natural := 0) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      F      : File_Type;
      Buffer : Stream_Element_Array (1 .. 10_000);
      Pos    : Positive_Count := 1;
      Last   : Stream_Element_Offset;
      Space  : Natural;
   begin
      if Max_Size = 0 then
         Space := Natural'Last;
      else
         Space := Max_Size;
      end if;
      Open (Name => Path, File => F, Mode => In_File);
      loop
         Read (File => F, Item => Buffer, From => Pos, Last => Last);
         if Natural (Last) > Space then
            Last := Stream_Element_Offset (Space);
         end if;
         for I in 1 .. Last loop
            Append (Into, Character'Val (Buffer (I)));
         end loop;
         exit when Last < Buffer'Length;
         Pos := Pos + Positive_Count (Last);
      end loop;
      Close (F);

   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end Read_File;

   --  ------------------------------
   --  Read the file with the given path, one line at a time and execute the <b>Process</b>
   --  procedure with each line as argument.
   --  ------------------------------
   procedure Read_File (Path     : in String;
                        Process  : not null access procedure (Line : in String)) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Path);
      while not Ada.Text_IO.End_Of_File (File) loop
         Process (Ada.Text_IO.Get_Line (File));
      end loop;
      Ada.Text_IO.Close (File);
   end Read_File;

   --  ------------------------------
   --  Read the file with the given path, one line at a time and append each line to
   --  the <b>Into</b> vector.
   --  ------------------------------
   procedure Read_File (Path  : in String;
                        Into  : in out Util.Strings.Vectors.Vector) is
      procedure Append (Line : in String);
      procedure Append (Line : in String) is
      begin
         Into.Append (Line);
      end Append;
   begin
      Read_File (Path, Append'Access);
   end Read_File;

   --  ------------------------------
   --  Save the string into a file creating the file if necessary
   --  ------------------------------
   procedure Write_File (Path    : in String;
                         Content : in String) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      use Ada.Directories;

      F : File_Type;
      Buffer : Stream_Element_Array (Stream_Element_Offset (Content'First)
                                     .. Stream_Element_Offset (Content'Last));

      Dir : constant String := Containing_Directory (Path);
   begin
      if not Exists (Dir) then
         Create_Path (Dir);
      end if;
      Create (File => F, Name => Path);
      for I in Content'Range loop
         Buffer (Stream_Element_Offset (I))
           := Stream_Element (Character'Pos (Content (I)));
      end loop;
      Write (F, Buffer);
      Close (F);

   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end Write_File;

   --  ------------------------------
   --  Save the string into a file creating the file if necessary
   --  ------------------------------
   procedure Write_File (Path    : in String;
                         Content : in Unbounded_String) is
   begin
      Write_File (Path, Ada.Strings.Unbounded.To_String (Content));
   end Write_File;

   --  ------------------------------
   --  Iterate over the search directories defined in <b>Paths</b> and execute
   --  <b>Process</b> with each directory until it returns <b>True</b> in <b>Done</b>
   --  or the last search directory is found.  Each search directory
   --  is separated by ';' (yes, even on Unix).  When <b>Going</b> is set to Backward, the
   --  directories are searched in reverse order.
   --  ------------------------------
   procedure Iterate_Path (Path     : in String;
                           Process  : not null access procedure (Dir  : in String;
                                                                 Done : out Boolean);
                           Going    : in Direction := Ada.Strings.Forward) is
   begin
      Util.Strings.Tokenizers.Iterate_Tokens (Content => Path,
                                              Pattern => ";",
                                              Process => Process,
                                              Going   => Going);
   end Iterate_Path;

   --  ------------------------------
   --  Find the file `Name` in one of the search directories defined in `Paths`.
   --  Each search directory is separated by ';' by default (yes, even on Unix).
   --  This can be changed by specifying the `Separator` value.
   --  Returns the path to be used for reading the file.
   --  ------------------------------
   function Find_File_Path (Name      : in String;
                            Paths     : in String;
                            Separator : in Character := ';') return String is
      Sep_Pos : Natural;
      Pos     : Positive := Paths'First;
      Last    : constant Natural := Paths'Last;
   begin
      while Pos <= Last loop
         Sep_Pos := Util.Strings.Index (Paths, Separator, Pos);
         if Sep_Pos = 0 then
            Sep_Pos := Last;
         else
            Sep_Pos := Sep_Pos - 1;
         end if;
         declare
            use Ada.Directories;

            Path : constant String := Util.Files.Compose (Paths (Pos .. Sep_Pos), Name);
         begin
            if Exists (Path) and then Kind (Path) = Ordinary_File then
               return Path;
            end if;
         exception
            when Name_Error =>
               null;
         end;
         Pos := Sep_Pos + 2;
      end loop;
      return Name;
   end Find_File_Path;

   --  ------------------------------
   --  Iterate over the search directories defined in <b>Path</b> and search
   --  for files matching the pattern defined by <b>Pattern</b>.  For each file,
   --  execute <b>Process</b> with the file basename and the full file path.
   --  Stop iterating when the <b>Process</b> procedure returns True.
   --  Each search directory is separated by ';'.  When <b>Going</b> is set to Backward, the
   --  directories are searched in reverse order.
   --  ------------------------------
   procedure Iterate_Files_Path (Pattern  : in String;
                                 Path     : in String;
                                 Process  : not null access procedure (Name : in String;
                                                                       File : in String;
                                                                       Done : out Boolean);
                                 Going    : in Direction := Ada.Strings.Forward) is

      procedure Find_Files (Dir  : in String;
                            Done : out Boolean);

      --  ------------------------------
      --  Find the files matching the pattern in <b>Dir</b>.
      --  ------------------------------
      procedure Find_Files (Dir  : in String;
                            Done : out Boolean) is
         use Ada.Directories;

         Filter  : constant Filter_Type := (Ordinary_File => True, others => False);
         Ent     : Directory_Entry_Type;
         Search  : Search_Type;
      begin
         Done := False;
         Start_Search (Search, Directory => Dir,
                       Pattern => Pattern, Filter => Filter);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Name      : constant String := Simple_Name (Ent);
               File_Path : constant String := Full_Name (Ent);
            begin
               Process (Name, File_Path, Done);
               exit when Done;
            end;
         end loop;
      end Find_Files;

   begin
      Iterate_Path (Path => Path, Process => Find_Files'Access, Going => Going);
   end Iterate_Files_Path;

   --  ------------------------------
   --  Find the files which match the pattern in the directories specified in the
   --  search path <b>Path</b>.  Each search directory is separated by ';'.
   --  File names are added to the string set in <b>Into</b>.
   --  ------------------------------
   procedure Find_Files_Path (Pattern : in String;
                              Path    : in String;
                              Into    : in out Util.Strings.Maps.Map) is

      procedure Add_File (Name : in String;
                          File_Path : in String;
                          Done : out Boolean);

      --  ------------------------------
      --  Find the files matching the pattern in <b>Dir</b>.
      --  ------------------------------
      procedure Add_File (Name : in String;
                          File_Path : in String;
                          Done      : out Boolean) is
      begin
         if not Into.Contains (Name) then
            Into.Insert (Name, File_Path);
         end if;
         Done := False;
      end Add_File;

   begin
      Iterate_Files_Path (Pattern => Pattern, Path => Path, Process => Add_File'Access);
   end Find_Files_Path;

   --  ------------------------------
   --  Compose an existing path by adding the specified name to each path component
   --  and return a new paths having only existing directories.  Each directory is
   --  separated by ';' (this can be overriding with the `Separator` parameter).
   --  If the composed path exists, it is added to the result path.
   --  Example:
   --    paths = 'web;regtests'  name = 'info'
   --    result = 'web/info;regtests/info'
   --  Returns the composed path.
   --  ------------------------------
   function Compose_Path (Paths     : in String;
                          Name      : in String;
                          Separator : in Character := ';') return String is

      procedure Compose (Dir  : in String;
                         Done : out Boolean);

      Result  : Util.Strings.Builders.Builder (256);

      --  ------------------------------
      --  Build the new path by checking if <b>Name</b> exists in <b>Dir</b>
      --  and appending the new path in the <b>Result</b>.
      --  ------------------------------
      procedure Compose (Dir  : in String;
                         Done : out Boolean) is
         use Ada.Directories;

         Path : constant String := Util.Files.Compose (Dir, Name);
      begin
         Done := False;
         if Exists (Path) and then Kind (Path) = Directory then
            if Util.Strings.Builders.Length (Result) > 0 then
               Util.Strings.Builders.Append (Result, Separator);
            end if;
            Util.Strings.Builders.Append (Result, Path);
         end if;
      exception
         when Name_Error =>
            null;
      end Compose;

   begin
      Iterate_Path (Path => Paths, Process => Compose'Access);
      return Util.Strings.Builders.To_Array (Result);
   end Compose_Path;

   --  ------------------------------
   --  Returns the name of the external file with the specified directory
   --  and the name.  Unlike the Ada.Directories.Compose, the name can represent
   --  a relative path and thus include directory separators.
   --  ------------------------------
   function Compose (Directory : in String;
                     Name      : in String) return String is
   begin
      if Name'Length = 0 then
         return Directory;
      elsif Directory'Length = 0 then
         return Name;
      elsif Directory = "." or else Directory = "./" then
         if Name (Name'First) = '/' then
            return Compose (Directory, Name (Name'First + 1 .. Name'Last));
         else
            return Name;
         end if;
      elsif Directory (Directory'Last) = '/' and then Name (Name'First) = '/' then
         return Directory & Name (Name'First + 1 .. Name'Last);
      elsif Directory (Directory'Last) = '/' or else Name (Name'First) = '/' then
         return Directory & Name;
      else
         return Directory & "/" & Name;
      end if;
   end Compose;

   --  ------------------------------
   --  Returns a relative path whose origin is defined by <b>From</b> and which refers
   --  to the absolute path referenced by <b>To</b>.  Both <b>From</b> and <b>To</b> are
   --  assumed to be absolute paths.  Returns the absolute path <b>To</b> if the relative
   --  path could not be found.  Both paths must have at least one root component in common.
   --  ------------------------------
   function Get_Relative_Path (From : in String;
                               To   : in String) return String is
      Result : Unbounded_String;
      Last   : Natural := 0;
   begin
      for I in From'Range loop
         if I > To'Last or else From (I) /= To (I) then
            --  Nothing in common, return the absolute path <b>To</b>.
            if Last <= From'First + 1 then
               return To;
            end if;

            for J in Last .. From'Last - 1 loop
               if From (J) = '/' or else From (J) = '\' then
                  Append (Result, "../");
               end if;
            end loop;
            if Last <= To'Last and then From (I) /= '/' and then From (I) /= '\' then
               Append (Result, "../");
               Append (Result, To (Last .. To'Last));
            end if;
            return To_String (Result);

         elsif I < From'Last and then (From (I) = '/' or else From (I) = '\') then
            Last := I + 1;

         end if;
      end loop;
      if To'Last = From'Last
        or else (To'Last = From'Last + 1
                   and then (To (To'Last) = '/' or else To (To'Last) = '\'))
      then
         return ".";
      elsif Last = 0 then
         return To;
      elsif To (From'Last + 1) = '/' or else To (From'Last + 1) = '\' then
         return To (From'Last + 2 .. To'Last);
      else
         return To (Last .. To'Last);
      end if;
   end Get_Relative_Path;

   --  ------------------------------
   --  Rename the old name into a new name.
   --  ------------------------------
   procedure Rename (Old_Name, New_Name : in String) is
      --  Rename a file (the Ada.Directories.Rename does not allow to use the
      --  Unix atomic file rename!)

      C_Old_Path : constant String := Old_Name & ASCII.NUL;
      C_New_Path : constant String := New_Name & ASCII.NUL;
      Result     : Integer;
   begin
      --  Do a system atomic rename of old file in the new file.
      --  Ada.Directories.Rename does not allow this.
      Result := Util.Systems.Os.Sys_Rename (C_Old_Path, C_New_Path);
      if Result /= 0 then
         raise Ada.IO_Exceptions.Use_Error with "Cannot rename file";
      end if;
   end Rename;

   --  ------------------------------
   --  Delete the file including missing symbolic link
   --  or socket files (which GNAT fails to delete,
   --  see gcc/63222 and gcc/56055).
   --  ------------------------------
   function Delete_File (Path : in String) return Integer is
      C_Path : constant String := Path & ASCII.NUL;
   begin
      if Util.Systems.Os.Sys_Unlink (C_Path) = 0 then
         return 0;
      else
         return Util.Systems.Os.Errno;
      end if;
   end Delete_File;

   procedure Delete_File (Path : in String) is
      Result : constant Integer := Delete_File (Path);
   begin
      if Result /= 0 then
         raise Ada.IO_Exceptions.Use_Error with "file """ & Path & """ could not be deleted";
      end if;
   end Delete_File;

   --  ------------------------------
   --  Delete the directory tree recursively.  If the directory tree contains
   --  sockets, special files and dangling symbolic links, they are removed
   --  correctly.  This is a workaround for GNAT bug gcc/63222 and gcc/56055.
   --  ------------------------------
   procedure Delete_Tree (Path : in String) is
      use type System.Address;
      use Util.Systems.Types;
      use Util.Systems.Os;
      use Interfaces.C;

      C_Path         : constant String := Path & ASCII.NUL;
      Dirp           : Util.Systems.Os.DIR;
      Buffer         : String (1 .. 1024);
      Length         : aliased Integer;
      File_Name_Addr : System.Address;
      Result         : Integer;
      St             : aliased Util.Systems.Types.Stat_Type;
   begin
      Dirp := Util.Systems.Os.Opendir (C_Path);
      if Dirp = Util.Systems.Os.Null_Dir then
         raise Ada.Directories.Use_Error with "unreadable directory """ & Path & '"';
      end if;

      begin
         loop
            File_Name_Addr := Util.Systems.Os.Readdir (Dirp, Buffer'Address, Length'Access);
            exit when File_Name_Addr = System.Null_Address;
            declare
               subtype File_Name_String is String (1 .. Length);

               File_Name : constant File_Name_String
                 with Import, Address => File_Name_Addr;

            begin
               if File_Name /= "." and then File_Name /= ".." then
                  declare
                     File_Path : constant String
                       := Path & Directory_Separator & File_Name & ASCII.NUL;
                  begin
                     Result := Util.Systems.Os.Sys_Lstat (File_Path, St'Unchecked_Access);
                     if Result = 0 and then (St.st_mode and S_IFMT) = S_IFDIR then
                        Delete_Tree (File_Path (File_Path'First .. File_Path'Last - 1));
                     else
                        Result := Util.Systems.Os.Sys_Unlink (File_Path);
                     end if;
                  end;
               end if;
            end;
         end loop;
         Result := Util.Systems.Os.Closedir (Dirp);

      exception
         when others =>
            Result := Util.Systems.Os.Closedir (Dirp);
            raise;

      end;
      Ada.Directories.Delete_Directory (Path);
   end Delete_Tree;

   --  ------------------------------
   --  Find the canonicalized absolute path of the given file.
   --  ------------------------------
   function Realpath (Path : in String) return String is
      use Interfaces.C.Strings;

      P : chars_ptr := New_String (Path);
      R : chars_ptr;
   begin
      R := Util.Systems.Os.Sys_Realpath (P, Null_Ptr);
      Free (P);
      if R = Null_Ptr then
         raise Ada.Directories.Use_Error with "invalid file """ & Path & '"';
      end if;
      declare
         Result : constant String := Value (R);
      begin
         Free (R);
         return Result;
      end;
   end Realpath;

end Util.Files;
