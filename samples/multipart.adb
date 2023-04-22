with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Util.Streams.Files;
with Util.Streams.Buffered.Parts;
procedure Multipart is

   procedure Extract_Cert (Source      : in String);

   procedure Extract_Cert (Source      : in String) is
      In_Stream   : aliased Util.Streams.Files.File_Stream;
      Part_Stream : Util.Streams.Buffered.Parts.Input_Part_Stream;
      Drop        : Ada.Streams.Stream_Element;
      Content     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      In_Stream.Open (Mode => Ada.Streams.Stream_IO.In_File, Name => Source);
      Part_Stream.Initialize (Input => In_Stream'Unchecked_Access, Size => 4096);
      Part_Stream.Set_Boundary ("-----BEGIN CERTIFICATE-----" & ASCII.LF);
      while not Part_Stream.Is_Eob loop
         Part_Stream.Read (Drop);
      end loop;
      Part_Stream.Set_Boundary ("-----END CERTIFICATE-----");
      Part_Stream.Read (Content);
      Ada.Text_IO.Put (Ada.Strings.Unbounded.To_String (Content));

   exception
      when Ada.IO_Exceptions.Data_Error =>
         Ada.Text_IO.Put_Line ("It seems this is not a PEM certificate file");
   end Extract_Cert;

begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: multipart certificate.pem");
      Ada.Text_IO.Put_Line ("Example: multipart ./samples/ISRG_Root_X1.pem");
      return;
   end if;

   Extract_Cert (Source => Ada.Command_Line.Argument (1));
end Multipart;
