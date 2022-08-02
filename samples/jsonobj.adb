with Ada.Text_IO;
with Util.Serialize.IO.JSON;
with Util.Beans.Objects;
with Util.Beans.Objects.Vectors;
with Util.Beans.Objects.Maps;
with Util.Streams.Texts;
with Util.Streams.Buffered;
procedure JsonObj is

   package UBO renames Util.Beans.Objects;
   procedure Add_Person (Into : in UBO.Vectors.Vector_Bean_Access;
                         Name : in String;
                         Last_Name : in String;
                         Age : in Natural);

   procedure Add_Person (Into : in UBO.Vectors.Vector_Bean_Access;
                         Name : in String;
                         Last_Name : in String;
                         Age : in Natural) is
      Person : constant UBO.Object := UBO.Maps.Create;
   begin
      UBO.Set_Value (Person, "name", UBO.To_Object (Name));
      UBO.Set_Value (Person, "last_name", UBO.To_Object (Last_Name));
      UBO.Set_Value (Person, "age", UBO.To_Object (Age));
      Into.Append (Person);
   end Add_Person;

   List   : constant UBO.Vectors.Vector_Bean_Access := new UBO.Vectors.Vector_Bean;
   Root   : constant UBO.Object := UBO.To_Object (List);
begin
   Add_Person (List, "John", "Johnson", 23);
   Add_Person (List, "Harry", "Potter", 17);

   declare
      Buffer : aliased Util.Streams.Buffered.Output_Buffer_Stream;
      Print  : aliased Util.Streams.Texts.Print_Stream;
      Output : Util.Serialize.IO.JSON.Output_Stream;
   begin
      Buffer.Initialize (Size => 10000);
      Print.Initialize (Buffer'Unchecked_Access);
      Output.Initialize (Print'Unchecked_Access);

      Output.Write_Entity ("", Root);
      Output.Flush;
      Ada.Text_IO.Put_Line (Util.Streams.Texts.To_String (Buffer));
   end;
end JsonObj;
