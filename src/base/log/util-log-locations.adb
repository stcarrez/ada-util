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
with Util.Strings;
package body Util.Log.Locations is

   --  ------------------------------
   --  Create a <b>File_Info</b> record to identify the file whose path is <b>Path</b>
   --  and whose relative path portion starts at <b>Relative_Position</b>.
   --  ------------------------------
   function Create_File_Info (Path              : in String;
                              Relative_Position : in Natural) return File_Info_Access is
   begin
      return new File_Info '(Length       => Path'Length,
                             Path         => Path,
                             Relative_Pos => Relative_Position);
   end Create_File_Info;

   --  ------------------------------
   --  Get the relative path name
   --  ------------------------------
   function Relative_Path (File : in File_Info) return String is
   begin
      return File.Path (File.Relative_Pos .. File.Path'Last);
   end Relative_Path;

   --  ------------------------------
   --  Get the line number
   --  ------------------------------
   function Line (Info : Line_Info) return Natural is
   begin
      return Info.Line;
   end Line;

   --  ------------------------------
   --  Get the column number
   --  ------------------------------
   function Column (Info : Line_Info) return Natural is
   begin
      return Info.Column;
   end Column;

   --  ------------------------------
   --  Get the source file
   --  ------------------------------
   function File (Info : Line_Info) return String is
   begin
      return Info.File.Path;
   end File;

   --  ------------------------------
   --  Compare the two source location.  The comparison is made on:
   --  o the source file,
   --  o the line number,
   --  o the column.
   --  ------------------------------
   function "<" (Left, Right : in Line_Info) return Boolean is
   begin
      if Left.File.Path < Right.File.Path then
         return True;
      elsif Left.File.Path > Right.File.Path then
         return False;
      elsif Left.Line < Right.Line then
         return True;
      elsif Left.Line > Right.Line then
         return False;
      else
         return Left.Column < Right.Column;
      end if;
   end "<";

   --  ------------------------------
   --  Create a source line information.
   --  ------------------------------
   function Create_Line_Info (File   : in File_Info_Access;
                              Line   : in Natural;
                              Column : in Natural := 0) return Line_Info is
      Result : Line_Info;
   begin
      Result.Line   := Line;
      Result.Column := Column;
      if File = null then
         Result.File := NO_FILE'Access;
      else
         Result.File := File;
      end if;
      return Result;
   end Create_Line_Info;

   --  ------------------------------
   --  Get a printable representation of the line information using
   --  the format:
   --    <path>:<line>[:<column>]
   --  The path can be reported as relative or absolute path.
   --  The column is optional and reported by default.
   --  ------------------------------
   function To_String (Info     : in Line_Info;
                       Relative : in Boolean := True;
                       Column   : in Boolean := True) return String is
   begin
      if Relative then
         if Column then
            return Relative_Path (Info.File.all) & ":" & Util.Strings.Image (Info.Line)
               & ":" & Util.Strings.Image (Info.Column);
         else
            return Relative_Path (Info.File.all) & ":" & Util.Strings.Image (Info.Line);
         end if;
      else
         if Column then
            return Info.File.Path & ":" & Util.Strings.Image (Info.Line)
               & ":" & Util.Strings.Image (Info.Column);
         else
            return Info.File.Path & ":" & Util.Strings.Image (Info.Line);
         end if;
      end if;
   end To_String;

end Util.Log.Locations;
