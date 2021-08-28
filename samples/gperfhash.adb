-----------------------------------------------------------------------
--  gperfhash -- Perfect hash Ada generator
--  Copyright (C) 2011, 2012, 2013, 2018, 2021 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;
with Ada.Command_Line;

with GNAT.Command_Line;
with GNAT.Perfect_Hash_Generators;

with Util.Log.Loggers;
with Util.Files;
with Util.Strings.Vectors;
with Util.Strings.Transforms;

--  This simple utility is an Ada perfect hash generator.  Given a fixed set of keywords,
--  it generates an Ada package (spec and body) which provides a perfect hash function.
--  The perfect hash algorithm is in fact provided by GNAT Perfect_Hash_Generators package.
--
--  Usage: gperfhash [-i] [-p package] keyword-file
--
--  -i           Generate a perfect hash which ignores the case
--  -p package   Use <b>package</b> as the name of package (default is <b>gphash</b>)
--  keyword-file The file which contains the keywords, one keyword on each line
procedure Gperfhash is

   use Util.Log.Loggers;
   use Ada.Strings.Unbounded;
   use GNAT.Command_Line;
   use Ada.Text_IO;

   --  Read a keyword and add it in the keyword list.
   procedure Read_Keyword (Line : in String);

   --  Given a package name, return the file name that correspond.
   function To_File_Name (Name : in String) return String;

   procedure Generate_Keyword_Table (Into : in out Ada.Text_IO.File_Type);

   --  Generate the package specification.
   procedure Generate_Specs (Name : in String);

   --  Generate the package body.
   procedure Generate_Body (Name : in String);

   Log         : constant Logger := Create ("log");

   Pkg_Name    : Unbounded_String := To_Unbounded_String ("gphash");
   Names       : Util.Strings.Vectors.Vector;

   --  When true, generate a perfect hash which ignores the case.
   Ignore_Case : Boolean := False;

   --  ------------------------------
   --  Generate the keyword table.
   --  ------------------------------
   procedure Generate_Keyword_Table (Into : in out Ada.Text_IO.File_Type) is

      Index : Integer := 0;

      procedure Print_Keyword (Pos : in Util.Strings.Vectors.Cursor);
      procedure Print_Table (Pos : in Util.Strings.Vectors.Cursor);

      --  ------------------------------
      --  Print a keyword as an Ada constant string.
      --  ------------------------------
      procedure Print_Keyword (Pos : in Util.Strings.Vectors.Cursor) is
         Name : constant String := Util.Strings.Vectors.Element (Pos);
      begin
         Put (Into, "   K_");
         Put (Into, Util.Strings.Image (Index));
         Set_Col (Into, 20);
         Put (Into, ": aliased constant String := """);
         Put (Into, Name);
         Put_Line (Into, """;");
         Index := Index + 1;
      end Print_Keyword;

      --  ------------------------------
      --  Build the keyword table.
      --  ------------------------------
      procedure Print_Table (Pos : in Util.Strings.Vectors.Cursor) is
         pragma Unreferenced (Pos);
      begin
         if Index > 0 then
            if Index mod 4 = 0 then
               Put_Line (Into, ",");
               Put (Into, "      ");
            else
               Put (Into, ", ");
            end if;
         end if;
         Put (Into, "K_");
         Put (Into, Util.Strings.Image (Index));
         Put (Into, "'Access");
         Index := Index + 1;
      end Print_Table;

   begin
      New_Line (Into);
      Put_Line (Into, "   type Name_Access is access constant String;");
      Put_Line (Into, "   type Keyword_Array is array (Natural range <>) of Name_Access;");
      Put_Line (Into, "   Keywords : constant Keyword_Array;");
      Put_Line (Into, "private");
      New_Line (Into);
      Names.Iterate (Print_Keyword'Access);
      New_Line (Into);

      Index := 0;
      Put_Line (Into, "   Keywords : constant Keyword_Array := (");
      Put (Into, "      ");
      Names.Iterate (Print_Table'Access);
      Put_Line (Into, ");");
   end Generate_Keyword_Table;

   --  ------------------------------
   --  Generate the package specification.
   --  ------------------------------
   procedure Generate_Specs (Name : in String) is
      File : Ada.Text_IO.File_Type;
      Path : constant String := To_File_Name (Name) & ".ads";
   begin
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.Out_File,
                        Name => Path);
      Put_Line (File, "--  Generated by gperfhash");
      Put (File, "package ");
      Put (File, To_String (Pkg_Name));
      Put_Line (File, " is");
      New_Line (File);
      Put_Line (File, "   pragma Preelaborate;");
      New_Line (File);
      Put_Line (File, "   function Hash (S : String) return Natural;");
      New_Line (File);
      Put_Line (File, "   --  Returns true if the string <b>S</b> is a keyword.");
      Put_Line (File, "   function Is_Keyword (S : in String) return Boolean;");
      Generate_Keyword_Table (File);
      Put      (File, "end ");
      Put      (File, To_String (Pkg_Name));
      Put      (File, ";");
      New_Line (File);
      Close    (File);
   end Generate_Specs;

   --  ------------------------------
   --  Generate the package body.
   --  ------------------------------
   procedure Generate_Body (Name : in String) is

      --  Read the generated body file.
      procedure Read_Body (Line : in String);

      Path  : constant String := To_File_Name (Name) & ".adb";
      File  : Ada.Text_IO.File_Type;
      Count : Natural;
      Lines : Util.Strings.Vectors.Vector;

      --  ------------------------------
      --  Read the generated body file.
      --  ------------------------------
      procedure Read_Body (Line : in String) is
      begin
         Lines.Append (Line);
      end Read_Body;

   begin
      Util.Files.Read_File (Path => Path, Process => Read_Body'Access);
      Count := Natural (Lines.Length);
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.Out_File,
                        Name => Path);
      Put_Line (File, "--  Generated by gperfhash");
      if Ignore_Case then
         Put_Line (File, "with Util.Strings.Transforms;");
      end if;
      for I in 1 .. Count loop
         declare
            L : constant String := Lines.Element (I);
         begin
            Put_Line (File, L);

            --  Generate the Is_Keyword function before the package end.
            if I = Count - 1 then
               Put_Line (File, "   --  Returns true if the string <b>S</b> is a keyword.");
               Put_Line (File, "   function Is_Keyword (S : in String) return Boolean is");
               if Ignore_Case then
                  Put_Line (File, "      K : constant String := "
                            & "Util.Strings.Transforms.To_Upper_Case (S);");
                  Put_Line (File, "      H : constant Natural := Hash (K);");
               else
                  Put_Line (File, "      H : constant Natural := Hash (S);");
               end if;
               Put_Line (File, "   begin");
               if Ignore_Case then
                  Put_Line (File, "      return Keywords (H).all = K;");
               else
                  Put_Line (File, "      return Keywords (H).all = S;");
               end if;
               Put_Line (File, "   end Is_Keyword;");
            end if;
         end;
      end loop;
      Close (File);
   end Generate_Body;

   --  ------------------------------
   --  Read a keyword and add it in the keyword list.
   --  ------------------------------
   procedure Read_Keyword (Line : in String) is
      use Ada.Strings;
      Word : String := Fixed.Trim (Line, Both);
   begin
      if Word'Length > 0 then
         if Ignore_Case then
            Word := Util.Strings.Transforms.To_Upper_Case (Word);
         end if;
         Names.Append (Word);
         GNAT.Perfect_Hash_Generators.Insert (Word);
      end if;
   end Read_Keyword;

   --  ------------------------------
   --  Given a package name, return the file name that correspond.
   --  ------------------------------
   function To_File_Name (Name : in String) return String is
      Result : String (Name'Range);
   begin
      for J in Name'Range loop
         if Name (J) in 'A' .. 'Z' then
            Result (J) := Character'Val (Character'Pos (Name (J))
              - Character'Pos ('A')
              + Character'Pos ('a'));

         elsif Name (J) = '.' then
            Result (J) := '-';

         else
            Result (J) := Name (J);
         end if;
      end loop;
      return Result;
   end To_File_Name;

begin
   --  Initialization is optional.  Get the log configuration by reading the property
   --  file 'samples/log4j.properties'.  The 'log.util' logger will use a DEBUG level
   --  and write the message in 'result.log'.
   Util.Log.Loggers.Initialize ("samples/log4j.properties");

   loop
      case Getopt ("h i p: package: help") is
         when ASCII.NUL =>
            exit;

         when 'i' =>
            Ignore_Case := True;

         when 'p' =>
            Pkg_Name := To_Unbounded_String (Parameter);

         when others =>
            raise GNAT.Command_Line.Invalid_Switch;
      end case;
   end loop;
   declare
      Keywords : constant String := Get_Argument;
      Pkg      : constant String := To_String (Pkg_Name);
      Count    : Natural := 0;
      K_2_V    : Float;
      V        : Natural;
      Seed     : constant Natural := 4321; --  Needed by the hash algorithm
   begin
      --  Read the keywords.
      Util.Files.Read_File (Path => Keywords, Process => Read_Keyword'Access);

      Count := Natural (Names.Length);
      if Count = 0 then
         Log.Error ("There is no keyword.");
         raise GNAT.Command_Line.Invalid_Switch;
      end if;

      --  Generate the perfect hash package.
      V := 2 * Count + 1;
      loop
         K_2_V := Float (V) / Float (Count);
         GNAT.Perfect_Hash_Generators.Initialize (Seed, K_2_V);
         begin
            GNAT.Perfect_Hash_Generators.Compute;
            exit;
         exception
            when GNAT.Perfect_Hash_Generators.Too_Many_Tries =>
               V := V + 1;
         end;
      end loop;
      GNAT.Perfect_Hash_Generators.Produce (Pkg);

      --  Override what GNAT generates to have a list of keywords and other operations.
      Generate_Specs (Pkg);
      Generate_Body (Pkg);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Cannot read the keyword file.");
         Ada.Command_Line.Set_Exit_Status (1);
   end;

exception
   when GNAT.Command_Line.Invalid_Switch =>
      Log.Error ("Usage: gperfhash -i -p package keyword-file");
      Log.Error ("-i           Generate a perfect hash which ignores the case");
      Log.Error ("-p package   Use 'package' as the name of package (default is 'gphash')");
      Log.Error ("keyword-file The file which contains the keywords, one keyword on each line");
      Ada.Command_Line.Set_Exit_Status (1);
end Gperfhash;
