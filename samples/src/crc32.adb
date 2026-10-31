with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Interfaces;
with Util.Streams.Files;
with Util.Encoders.CRC32;
procedure CRC32 is

   procedure Hash_File (Source : in String;
                        Hash   : out Util.Encoders.CRC32.Digest);

   procedure Hash_File (Source : in String;
                        Hash   : out Util.Encoders.CRC32.Digest) is
      use type Ada.Streams.Stream_Element_Offset;

      In_Stream : Util.Streams.Files.File_Stream;
      Context   : Util.Encoders.CRC32.Context;
      Data      : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last      : Ada.Streams.Stream_Element_Offset;
   begin
      In_Stream.Open (Mode => Ada.Streams.Stream_IO.In_File, Name => Source);
      loop
         In_Stream.Read (Data, Last);
         exit when Last < Data'First;
         Context.Update (Data (Data'First .. Last));
      end loop;
      Context.Finish (Hash);
   end Hash_File;

   Result : Util.Encoders.CRC32.Digest;
   Hex    : Util.Encoders.Encoder := Util.Encoders.Create (Util.Encoders.BASE_16);
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: crc32 file");
      return;
   end if;

   Hash_File (Source => Ada.Command_Line.Argument (1),
              Hash => Result);
   Ada.Text_IO.Put_Line (Util.Encoders.Encode_Unsigned_32 (Hex, Result));
end CRC32;
