with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Util.Streams.Files;
with Util.Encoders.SHA256;
procedure SHA256 is

   procedure Hash_File (Source : in String;
                        Hash   : out Util.Encoders.SHA256.Digest);

   procedure Hash_File (Source : in String;
                        Hash   : out Util.Encoders.SHA256.Digest) is
      use type Ada.Streams.Stream_Element_Offset;

      In_Stream : Util.Streams.Files.File_Stream;
      Context   : Util.Encoders.SHA256.Context;
      Data      : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last      : Ada.Streams.Stream_Element_Offset;
   begin
      In_Stream.Open (Mode => Ada.Streams.Stream_IO.In_File, Name => Source);
      loop
         In_Stream.Read (Data, Last);
         exit when Last < Data'First;
         Util.Encoders.SHA256.Update (Context, Data (Data'First .. Last));
      end loop;
      Util.Encoders.SHA256.Finish (Context, Hash);
   end Hash_File;

   Result : Util.Encoders.SHA256.Digest;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: sha256 file");
      return;
   end if;

   Hash_File (Source => Ada.Command_Line.Argument (1), Hash => Result);
   Ada.Text_IO.Put_Line (Result);
end SHA256;
