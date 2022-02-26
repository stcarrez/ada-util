with Ada.Text_IO;
with Util.Serialize.IO.XML;
with Util.Streams.Texts;
procedure Serialize_Xml is
   Output : aliased Util.Streams.Texts.Print_Stream;
   Stream : Util.Serialize.IO.XML.Output_Stream;
begin
   Output.Initialize (Size => 10000);
   Stream.Initialize (Output => Output'Unchecked_Access);
   --  Stream.Start_Document;
   Stream.Start_Entity ("person");
   Stream.Write_Entity ("name", "Harry Potter");
   Stream.Write_Entity ("gender", "male");
   Stream.Write_Entity ("age", 17);
   Stream.End_Entity ("person");
   --  Stream.End_Document;
   Ada.Text_IO.Put_Line (Util.Streams.Texts.To_String (Output));
end Serialize_Xml;
