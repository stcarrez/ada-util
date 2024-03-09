with GNAT.IO;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.UTF_Encoding;
with Util.Files.Walk;
with Util.Strings;
--  with Util.Log.Loggers;
procedure Tree is

   use GNAT.IO;
   use Ada.Strings.UTF_Encoding;
   use Ada.Strings.Unbounded;

   SEP_MARKER : constant UTF_8_String := "│   ";
   MID_MARKER : constant UTF_8_String := "├── ";
   END_MARKER : constant UTF_8_String := "└── ";

   package String_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Element_Type => String);

   subtype Filter_Context_Type is Util.Files.Walk.Filter_Context_Type;
   subtype Filter_Result is Util.Files.Walk.Filter_Result;

   type Walker_Type is new Util.Files.Walk.Walker_Type with record
      Files       : String_Sets.Set;
      Dirs        : String_Sets.Set;
      Prefix      : Unbounded_String;
      Ptr_Prefix  : Unbounded_String;
      Seg_Prefix  : Unbounded_String;
      File_Count  : Natural := 0;
      Dir_Count   : Natural := 0;
   end record;

   overriding
   function Get_Ignore_Path (Walker : Walker_Type;
                             Path   : String) return String;

   overriding
   procedure Scan_File (Walker : in out Walker_Type;
                        Path   : String);

   overriding
   procedure Scan_Subdir (Walker  : in out Walker_Type;
                          Path    : String;
                          Filter  : Filter_Context_Type;
                          Match   : Filter_Result);

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
      Walker.Files.Include (Ada.Directories.Simple_Name (Path));
   end Scan_File;

   overriding
   procedure Scan_Subdir (Walker  : in out Walker_Type;
                          Path    : String;
                          Filter  : Filter_Context_Type;
                          Match   : Filter_Result) is
   begin
      Walker.Dirs.Include (Path);
   end Scan_Subdir;

   overriding
   procedure Scan_Directory (Walker : in out Walker_Type;
                             Path   : String;
                             Filter : Filter_Context_Type) is
      Prefix  : constant Unbounded_String := Walker.Prefix;
      Segment : constant Unbounded_String := Walker.Seg_Prefix;
   begin
      Put_Line (To_String (Prefix) & To_String (Walker.Ptr_Prefix)
                & Ada.Directories.Simple_Name (Path));
      Walker.Files.Clear;
      Walker.Dirs.Clear;
      Walker.Prefix := Prefix & Segment;
      Util.Files.Walk.Walker_Type (Walker).Scan_Directory (Path, Filter);
      declare
         Dirs       : constant String_Sets.Set := Walker.Dirs;
         Dir_Count  : constant Natural := Natural (Walker.Dirs.Length);
         File_Count : constant Natural := Natural (Walker.Files.Length);
         Pos        : Natural := 0;
      begin
         Walker.File_Count := Walker.File_Count + File_Count;
         Walker.Dir_Count := Walker.Dir_Count + Dir_Count;
         for File of Walker.Files loop
            Pos := Pos + 1;
            if Pos = File_Count + Dir_Count then
               Put_Line (To_String (Walker.Prefix) & END_MARKER & File);
            else
               Put_Line (To_String (Walker.Prefix) & MID_MARKER & File);
            end if;
         end loop;
         for Dir of Dirs loop
            Pos := Pos + 1;
            Walker.Dirs.Clear;
            if Pos = File_Count + Dir_Count then
               Walker.Seg_Prefix := To_Unbounded_String ("    ");
               Walker.Ptr_Prefix := To_Unbounded_String (END_MARKER);
            else
               Walker.Seg_Prefix := To_Unbounded_String (SEP_MARKER);
               Walker.Ptr_Prefix := To_Unbounded_String (MID_MARKER);
            end if;
            Util.Files.Walk.Walker_Type (Walker).Scan_Subdir
              (Dir, Filter, Util.Files.Walk.Path_Filter.NO_MATCH);
         end loop;
      end;
      Walker.Prefix := Prefix;
   end Scan_Directory;

   W      : Walker_Type;
   Filter : Util.Files.Walk.Filter_Type;
   Count  : constant Natural := Ada.Command_Line.Argument_Count;
begin
   --  Util.Log.Loggers.Initialize ("samples/log4j.properties");
   W.Prefix := To_Unbounded_String ("");
   W.Seg_Prefix := To_Unbounded_String ("");
   for I in 1 .. Count loop
      W.Scan (Ada.Command_Line.Argument (I), Filter);
   end loop;
   Put_Line (Util.Strings.Image (W.Dir_Count) & " directories, "
               & Util.Strings.Image (W.File_Count) & " files");
end Tree;
