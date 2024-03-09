-----------------------------------------------------------------------
--  util-files-tests -- Unit tests for files
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2019, 2022, 2024 Stephane Carrez
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
with Ada.Directories;
with Ada.Text_IO;
with Util.Assertions;
with Util.Systems.Constants;
with Util.Strings.Sets;
with Util.Test_Caller;
with Util.Files.Walk;
with Util.Files.Filters;
package body Util.Files.Tests is

   use Util.Tests;

   procedure Assert_Equals is
     new Util.Assertions.Assert_Equals_T (Value_Type => Walk.Filter_Mode);

   package Caller is new Util.Test_Caller (Test, "Files");

   package String_Filters is
      new Util.Files.Filters (Element_Type => String);

   procedure Assert_Equals is
     new Util.Assertions.Assert_Equals_T (Value_Type => String_Filters.Match_Result);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Files.Read_File",
                       Test_Read_File'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Read_File (missing)",
                       Test_Read_File_Missing'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Read_File (truncate)",
                       Test_Read_File'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Write_File",
                       Test_Write_File'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Iterate_Path",
                       Test_Iterate_Path'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Find_File_Path",
                       Test_Find_File_Path'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Compose_Path",
                       Test_Compose_Path'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Get_Relative_Path",
                       Test_Get_Relative_Path'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Delete_Tree",
                       Test_Delete_Tree'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Realpath",
                       Test_Realpath'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Walk",
                       Test_Walk'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Walk.Filter",
                       Test_Filter'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Walk.Filter (Recursive)",
                       Test_Filter_Recursive'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Walk.Filter (Wildcard)",
                       Test_Filter_Wildcard'Access);
      Caller.Add_Test (Suite, "Test Util.Files.Filters.Insert",
                       Test_Path_Filter'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test reading a file into a string
   --  Reads this ada source file and checks we have read it correctly
   --  ------------------------------
   procedure Test_Read_File (T : in out Test) is
      Result : Unbounded_String;
   begin
      Read_File (Path => "regtests/util-files-tests.adb", Into => Result);
      T.Assert (Index (Result, "Util.Files.Tests") > 0,
                "Content returned by Read_File is not correct");
      T.Assert (Index (Result, "end Util.Files.Tests;") > 0,
                "Content returned by Read_File is not correct");
   end Test_Read_File;

   procedure Test_Read_File_Missing (T : in out Test) is
      Unused_Result : Unbounded_String;
   begin
      Read_File (Path => "regtests/files-test--util.adb", Into => Unused_Result);
      T.Assert (False, "No exception raised");
   exception
      when others =>
         null;
   end Test_Read_File_Missing;

   procedure Test_Read_File_Truncate (T : in out Test) is
      Result : Unbounded_String;
   begin
      Read_File (Path => "regtests/util-files-tests.adb", Into => Result,
                 Max_Size => 50);
      Assert_Equals (T, Length (Result), 50,
                     "Read_File did not truncate correctly");
      T.Assert (Index (Result, "Apache License") > 0,
                "Content returned by Read_File is not correct");
   end Test_Read_File_Truncate;

   --  ------------------------------
   --  Check writing a file
   --  ------------------------------
   procedure Test_Write_File (T : in out Test) is
      Path   : constant String := Util.Tests.Get_Test_Path ("test-write.txt");
      Content : constant String := "Testing Util.Files.Write_File" & ASCII.LF;
      Result : Unbounded_String;
   begin
      Write_File (Path => Path, Content => Content);
      Read_File (Path => Path, Into => Result);
      Assert_Equals (T, To_String (Result), Content,
                     "Invalid content written or read");
   end Test_Write_File;

   --  ------------------------------
   --  Check Find_File_Path
   --  ------------------------------
   procedure Test_Find_File_Path (T : in out Test) is
      Dir   : constant String := Util.Tests.Get_Path ("regtests");
      Paths : constant String := ".;" & Dir;
   begin
      declare
         P : constant String := Util.Files.Find_File_Path ("test.properties", Paths);
      begin
         Assert_Equals (T, Dir & "/test.properties", P,
                        "Invalid path returned");
      end;
      Assert_Equals (T, "blablabla.properties",
                     Util.Files.Find_File_Path ("blablabla.properties", Paths));
   end Test_Find_File_Path;

   --  ------------------------------
   --  Check Iterate_Path
   --  ------------------------------
   procedure Test_Iterate_Path (T : in out Test) is
      procedure Check_Path (Dir : in String;
                            Done : out Boolean);

      Last : Unbounded_String;

      procedure Check_Path (Dir : in String;
                            Done : out Boolean) is
      begin
         if Dir in "a" | "bc" | "de" then
            Done := False;
         else
            Done := True;
         end if;
         Last := To_Unbounded_String (Dir);
      end Check_Path;

   begin
      Iterate_Path ("a;bc;de;f", Check_Path'Access);
      Assert_Equals (T, "f", Last, "Invalid last path");

      Iterate_Path ("de;bc;de;b", Check_Path'Access);
      Assert_Equals (T, "b", Last, "Invalid last path");

      Iterate_Path ("de;bc;de;a", Check_Path'Access, Ada.Strings.Backward);
      Assert_Equals (T, "de", Last, "Invalid last path");

   end Test_Iterate_Path;

   --  ------------------------------
   --  Test the Compose_Path operation
   --  ------------------------------
   procedure Test_Compose_Path (T : in out Test) is
   begin
      Assert_Equals (T, "src/sys/processes/os-none",
                     Compose_Path ("src;regtests;src/sys/processes", "os-none"),
                     "Invalid path composition");
      Assert_Equals (T, "regtests/bundles",
                     Compose_Path ("src;regtests", "bundles"),
                     "Invalid path composition");
      if Ada.Directories.Exists ("/usr/bin") then
         Assert_Equals (T, "/usr/bin;/usr/local/bin;/usr/bin",
                        Compose_Path ("/usr;/usr/local;/usr", "bin"),
                        "Invalid path composition");
      end if;
   end Test_Compose_Path;

   --  ------------------------------
   --  Test the Get_Relative_Path operation.
   --  ------------------------------
   procedure Test_Get_Relative_Path (T : in out Test) is
   begin
      Assert_Equals (T, "../util",
                     Get_Relative_Path ("/home/john/src/asf", "/home/john/src/util"),
                     "Invalid relative path");
      Assert_Equals (T, "../util",
                     Get_Relative_Path ("/home/john/src/asf/", "/home/john/src/util"),
                     "Invalid relative path");
      Assert_Equals (T, "../util/b",
                     Get_Relative_Path ("/home/john/src/asf", "/home/john/src/util/b"),
                     "Invalid relative path");
      Assert_Equals (T, "../as",
                     Get_Relative_Path ("/home/john/src/asf", "/home/john/src/as"),
                     "Invalid relative path");
      Assert_Equals (T, "../../",
                     Get_Relative_Path ("/home/john/src/asf", "/home/john"),
                     "Invalid relative path");
      Assert_Equals (T, "/usr/share/admin",
                     Get_Relative_Path ("/home/john/src/asf", "/usr/share/admin"),
                     "Invalid absolute path");
      Assert_Equals (T, "/home/john",
                     Get_Relative_Path ("home/john/src/asf", "/home/john"),
                     "Invalid relative path");
      Assert_Equals (T, "e",
                     Get_Relative_Path ("/home/john/src/asf", "/home/john/src/asf/e"),
                     "Invalid relative path");
      Assert_Equals (T, ".",
                     Get_Relative_Path ("/home/john/src/asf", "/home/john/src/asf/"),
                     "Invalid relative path");
   end Test_Get_Relative_Path;

   function Sys_Symlink (Target : in System.Address; Link : in System.Address) return Integer
     with Import => True, Convention => C,
          Link_Name => Util.Systems.Constants.SYMBOL_PREFIX & "symlink";
   pragma Weak_External (Sys_Symlink);

   --  ------------------------------
   --  Test the Delete_Tree operation.
   --  ------------------------------
   procedure Test_Delete_Tree (T : in out Test) is
      use type System.Address;

      Path : constant String := Util.Tests.Get_Test_Path ("test-delete-tree");
   begin
      if Ada.Directories.Exists (Path) then
         Delete_Tree (Path);
      end if;

      --  Create a directory tree with symlink links that point to a non-existing file.
      Ada.Directories.Create_Directory (Path);
      for I in 1 .. 10 loop
         declare
            P : constant String := Compose (Path, Util.Strings.Image (I));
            S : String (1 .. P'Length + 3);
            R : Integer;
         begin
            Ada.Directories.Create_Directory (P);
            S (1 .. P'Length) := P;
            S (P'Length + 1) := '/';
            S (P'Length + 2) := 'A';
            S (S'Last) := ASCII.NUL;
            for J in 1 .. 5 loop
               Ada.Directories.Create_Path (Compose (P, Util.Strings.Image (J)));
            end loop;
            if Sys_Symlink'Address /= System.Null_Address then
               R := Sys_Symlink (S'Address, S'Address);
               Util.Tests.Assert_Equals (T, 0, R, "symlink creation failed");
            end if;
         end;
      end loop;
      T.Assert (Ada.Directories.Exists (Path), "Directory must exist");

      --  Ada.Directories.Delete_Tree (Path) fails to delete the tree.
      Delete_Tree (Path);
      T.Assert (not Ada.Directories.Exists (Path), "Directory must have been deleted");
   end Test_Delete_Tree;

   --  ------------------------------
   --  Test the Realpath function.
   --  ------------------------------
   procedure Test_Realpath (T : in out Test) is
      P : constant String := Util.Files.Realpath ("bin/util_harness");
   begin
      Util.Tests.Assert_Matches (T, ".*/bin/util_harness", P);
   end Test_Realpath;

   --  ------------------------------
   --  Test the Util.Files.Walk package
   --  ------------------------------
   procedure Test_Walk (T : in out Test) is
      subtype Filter_Context_Type is Util.Files.Walk.Filter_Context_Type;
      type Walker_Type is new Util.Files.Walk.Walker_Type with record
         Files : Util.Strings.Sets.Set;
         Dirs  : Util.Strings.Sets.Set;
      end record;

      overriding
      function Get_Ignore_Path (Walker : Walker_Type;
                                Path   : String) return String;

      overriding
      procedure Scan_File (Walker : in out Walker_Type;
                           Path   : String);

      overriding
      procedure Scan_Directory (Walker : in out Walker_Type;
                                Path   : String;
                                Filter : Filter_Context_Type);

      overriding
      function Get_Ignore_Path (Walker : Walker_Type;
                                Path   : String) return String is
         pragma Unreferenced (Walker);
      begin
         return Util.Files.Compose (Path, ".gitignore");
      end Get_Ignore_Path;

      overriding
      procedure Scan_File (Walker : in out Walker_Type;
                           Path   : String) is
      begin
         Walker.Files.Insert (Path);
      end Scan_File;

      overriding
      procedure Scan_Directory (Walker : in out Walker_Type;
                                Path   : String;
                                Filter : Filter_Context_Type) is
      begin
         Walker.Dirs.Insert (Path);
         Util.Files.Walk.Walker_Type (Walker).Scan_Directory (Path, Filter);
      end Scan_Directory;

      W : Walker_Type;
      Filter : Util.Files.Walk.Filter_Type;
   begin
      W.Scan ("./regtests", Filter);
      Ada.Text_IO.Put_Line ("Number of files:" & W.Files.Length'Image);
      for S of W.Files loop
         Ada.Text_IO.Put_Line (S);
      end loop;
      Ada.Text_IO.Put_Line ("Number of dirs:" & W.Dirs.Length'Image);
      for S of W.Dirs loop
         Ada.Text_IO.Put_Line (S);
      end loop;
      T.Assert (W.Files.Contains ("./regtests/util-files-tests.ads"),
                "Missing './regtests/util-files-tests.ads'");
      T.Assert (W.Files.Contains ("./regtests/util-files-tests.adb"),
                "Missing './regtests/util-files-tests.adb'");
      T.Assert (W.Dirs.Contains ("./regtests/files"),
                "Missing dir './regtests/files");
      T.Assert (not W.Dirs.Contains ("./bin"),
                "'bin' was scanned (see .gitignore)");
      T.Assert (not W.Dirs.Contains ("./alire"),
                "'alire' was scanned (see .gitignore)");
   end Test_Walk;

   --  ------------------------------
   --  Test the Util.Files.Filter operations.
   --  ------------------------------
   procedure Test_Path_Filter (T : in out Test) is
      F : String_Filters.Filter_Type;
   begin
      F.Insert ("/a/b/c/d", False, "a-b-c-d");
      F.Insert ("/a/b/d/e", False, "a-b-d-e");
      F.Insert ("/b/e/d", False, "b-e-d");
      F.Insert ("/a/b/d/f", False, "a-b-d-f");
      F.Insert ("/b/e/c", False, "b-e-c");
      F.Insert ("/a/c", False, "a-c");

      Assert_Equals (T, String_Filters.Found, F.Match ("a/b/c/d").Match);
      Assert_Equals (T, "a-b-c-d", String_Filters.Get_Value (F.Match ("a/b/c/d")));

      Assert_Equals (T, String_Filters.Found, F.Match ("a/b/d/e").Match);
      Assert_Equals (T, "a-b-d-e", String_Filters.Get_Value (F.Match ("a/b/d/e")));

      Assert_Equals (T, String_Filters.Found, F.Match ("b/e/d").Match);
      Assert_Equals (T, "b-e-d", String_Filters.Get_Value (F.Match ("b/e/d")));

      Assert_Equals (T, String_Filters.Found, F.Match ("a/b/d/f").Match);
      Assert_Equals (T, "a-b-d-f", String_Filters.Get_Value (F.Match ("a/b/d/f")));

      Assert_Equals (T, String_Filters.Found, F.Match ("b/e/c").Match);
      Assert_Equals (T, "b-e-c", String_Filters.Get_Value (F.Match ("b/e/c")));

      Assert_Equals (T, String_Filters.Found, F.Match ("a/c").Match);
      Assert_Equals (T, "a-c", String_Filters.Get_Value (F.Match ("a/c")));

      Assert_Equals (T, String_Filters.No_Value, F.Match ("a/b/c").Match);
      Assert_Equals (T, String_Filters.No_Value, F.Match ("a/b").Match);
      Assert_Equals (T, String_Filters.No_Value, F.Match ("b/e").Match);

      Assert_Equals (T, String_Filters.Not_Found, F.Match ("d/e").Match);

      F.Insert ("*.c", True, "C");
      Assert_Equals (T, String_Filters.Found, F.Match ("test.c").Match);
      Assert_Equals (T, "C", String_Filters.Get_Value (F.Match ("test.c")));

      Assert_Equals (T, String_Filters.Found, F.Match ("a/b/c/d/test.c").Match);
      Assert_Equals (T, "C", String_Filters.Get_Value (F.Match ("a/b/c/d/test.c")));

      F.Insert ("test/*.o", True, "O");
      Assert_Equals (T, String_Filters.Found, F.Match ("test/p.o").Match);
      Assert_Equals (T, "O", String_Filters.Get_Value (F.Match ("test/p.o")));

      Assert_Equals (T, String_Filters.Found, F.Match ("a/b/test/x.o").Match);
      Assert_Equals (T, "O", String_Filters.Get_Value (F.Match ("a/b/test/x.o")));

   end Test_Path_Filter;

   --  ------------------------------
   --  Test the Util.Files.Filter operations.
   --  ------------------------------
   procedure Test_Filter (T : in out Test) is
      F : Walk.Filter_Type;
   begin
      F.Include ("/a/b/c/d");
      F.Include ("/a/b/d/e");
      F.Include ("/b/e/d");
      F.Exclude ("/a/b/d/f");
      F.Exclude ("/b/e/c");
      F.Include ("/a/c");

      Assert_Equals (T, Walk.Included, F.Match ("a/b/c/d"));
      Assert_Equals (T, Walk.Included, F.Match ("a/b/d/e"));
      Assert_Equals (T, Walk.Included, F.Match ("a/c"));
      Assert_Equals (T, Walk.Excluded, F.Match ("a/b/d/f"));
      Assert_Equals (T, Walk.Excluded, F.Match ("b/e/c"));
      Assert_Equals (T, Walk.Included, F.Match ("a/c"));

      Assert_Equals (T, Walk.Not_Found, F.Match ("a"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("c"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("d/e"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("a/b/c/d/e"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("a/b/e"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("a/b/c"));

      F.Include ("/a/b/c");
      Assert_Equals (T, Walk.Included, F.Match ("a/b/c"));
      Assert_Equals (T, Walk.Included, F.Match ("a/b/c/d"));
      F.Exclude ("/a/b/c");
      Assert_Equals (T, Walk.Excluded, F.Match ("a/b/c"));
   end Test_Filter;

   --  ------------------------------
   --  Test the Util.Files.Filter operations.
   --  ------------------------------
   procedure Test_Filter_Recursive (T : in out Test) is
      F : Walk.Filter_Type;
   begin
      F.Include ("a/b/c/d");
      F.Include ("a/b/d/e");
      F.Include ("b/e/d");
      F.Exclude ("a/b/d/f");
      F.Exclude ("b/e/c");
      F.Include ("a/c");

      Assert_Equals (T, Walk.Included, F.Match ("a/b/c/d"));
      Assert_Equals (T, Walk.Included, F.Match ("a/b/d/e"));
      Assert_Equals (T, Walk.Included, F.Match ("a/c"));
      Assert_Equals (T, Walk.Excluded, F.Match ("a/b/d/f"));
      Assert_Equals (T, Walk.Excluded, F.Match ("b/e/c"));
      Assert_Equals (T, Walk.Included, F.Match ("a/c"));

      Assert_Equals (T, Walk.Included, F.Match ("x/y/a/b/c/d"));
      Assert_Equals (T, Walk.Included, F.Match ("q/w/e/a/b/d/e"));
      Assert_Equals (T, Walk.Included, F.Match ("t/y/u/a/c"));
      Assert_Equals (T, Walk.Excluded, F.Match ("i/a/b/d/f"));
      Assert_Equals (T, Walk.Excluded, F.Match ("o/b/e/c"));
      Assert_Equals (T, Walk.Included, F.Match ("z/a/c"));

      Assert_Equals (T, Walk.Not_Found, F.Match ("z/a"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("b/c"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("x/d/e"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("q/a/b/c/d/e"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("w/a/b/e"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("e/a/b/c"));

      F.Include ("a/b/c");
      Assert_Equals (T, Walk.Included, F.Match ("a/b/c"));
      Assert_Equals (T, Walk.Included, F.Match ("a/b/c/d"));
      F.Exclude ("a/b/c");
      Assert_Equals (T, Walk.Excluded, F.Match ("a/b/c"));
   end Test_Filter_Recursive;

   --  ------------------------------
   --  Test the Util.Files.Filter operations.
   --  ------------------------------
   procedure Test_Filter_Wildcard (T : in out Test) is
      F : Walk.Filter_Type;
   begin
      F.Include ("*.o");
      F.Include ("*.a");
      F.Include ("*.lib");
      F.Exclude ("a/b/*.o");
      F.Exclude ("b/e/*.a");
      F.Include ("a/**/*.c");

      Assert_Equals (T, Walk.Included, F.Match ("test.o"));
      Assert_Equals (T, Walk.Included, F.Match ("test.a"));
      Assert_Equals (T, Walk.Included, F.Match ("test.lib"));
      Assert_Equals (T, Walk.Excluded, F.Match ("a/b/test.o"));
      Assert_Equals (T, Walk.Excluded, F.Match ("b/e/test.a"));
      --  Assert_Equals (T, Walk.Included, F.Match ("a/c/d/e/test.c"));

      Assert_Equals (T, Walk.Included, F.Match ("x/y/a/b/c/d.o"));
      Assert_Equals (T, Walk.Included, F.Match ("q/w/e/a/b/d/e.a"));
      --  Assert_Equals (T, Walk.Included, F.Match ("t/y/u/a/b/x.c"));
      Assert_Equals (T, Walk.Excluded, F.Match ("i/a/b/x.o"));
      Assert_Equals (T, Walk.Excluded, F.Match ("o/b/e/c.a"));
      --  Assert_Equals (T, Walk.Included, F.Match ("z/a/t.c"));

      Assert_Equals (T, Walk.Not_Found, F.Match ("z/a"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("b/c"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("x/d/e"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("q/a/b/c/d/e"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("w/a/b/e"));
      Assert_Equals (T, Walk.Not_Found, F.Match ("e/a/b/c"));
   end Test_Filter_Wildcard;

end Util.Files.Tests;
