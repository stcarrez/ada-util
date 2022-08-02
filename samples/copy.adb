with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Util.Streams.Files;
procedure Copy is

   procedure Copy_File (Source      : in String;
                        Destination : in String);

   procedure Copy_File (Source      : in String;
                        Destination : in String) is
      In_Stream  : aliased Util.Streams.Files.File_Stream;
      Out_Stream : aliased Util.Streams.Files.File_Stream;
   begin
      In_Stream.Open (Mode => Ada.Streams.Stream_IO.In_File, Name => Source);
      Out_Stream.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Destination);
      Util.Streams.Copy (From => In_Stream, Into => Out_Stream);
   end Copy_File;

begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: copy source destination");
      return;
   end if;

   Copy_File (Source      => Ada.Command_Line.Argument (1),
              Destination => Ada.Command_Line.Argument (2));
end Copy;
