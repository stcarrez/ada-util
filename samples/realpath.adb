with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Directories;
with Util.Files;

procedure Realpath is
   Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   for I in 1 .. Count loop
      declare
         Path : constant String := Ada.Command_Line.Argument (I);
      begin
         Ada.Text_IO.Put_Line (Util.Files.Realpath (Path));

      exception
         when Ada.Directories.Use_Error =>
            Ada.Text_IO.Put_Line ("Invalid path: " & Path);
      end;
   end loop;
end Realpath;
