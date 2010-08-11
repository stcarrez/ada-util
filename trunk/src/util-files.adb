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
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Streams;
with Ada.Streams.Stream_IO;
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
   --  Find the file in one of the search directories.  Each search directory
   --  is separated by ';' (yes, even on Unix).
   --  Returns the path to be used for reading the file.
   --  ------------------------------
   function Find_File_Path (Name  : String;
                            Paths : String) return String is
      use Ada.Directories;
      use Ada.Strings.Fixed;

      Sep_Pos : Natural;
      Pos     : Positive := Paths'First;
      Last    : constant Natural := Paths'Last;
   begin
      while Pos <= Last loop
         Sep_Pos := Index (Paths, ";", Pos);
         if Sep_Pos = 0 then
            Sep_Pos := Last;
         else
            Sep_Pos := Sep_Pos - 1;
         end if;
         declare
            Dir  : constant String := Paths (Pos .. Sep_Pos);
            Path : constant String := Dir & "/" & Name;
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

end Util.Files;
