with Ada.Text_IO;
with Util.Serialize.IO.JSON;
with Util.Beans.Objects;
with Util.Beans.Objects.Vectors;
with Util.Beans.Objects.Datasets;
with Util.Streams.Texts;
with Util.Streams.Buffered;
procedure Datasets is

   package UBO renames Util.Beans.Objects;
   procedure Fill_Person (Row : in out UBO.Object_Array);
   procedure Fill_Number (Row : in out UBO.Object_Array);

   Set   : UBO.Datasets.Dataset_Access;

   procedure Fill_Person (Row : in out UBO.Object_Array) is
      Count : constant Natural := Set.Get_Count;
   begin
      Row (Row'First)     := UBO.To_Object (String '("john" & Natural'Image (Count)));
      Row (Row'First + 1) := UBO.To_Object (String '("jj@gmail.com"));
      Row (Row'First + 2) := UBO.To_Object (Count);
   end Fill_Person;

   procedure Fill_Number (Row : in out UBO.Object_Array) is
      Count : constant Natural := Set.Get_Count;
   begin
      Row (Row'First)     := UBO.To_Object (Count);
      Row (Row'First + 1) := UBO.To_Object (Count + Count);
   end Fill_Number;

   List   : constant UBO.Vectors.Vector_Bean_Access := new UBO.Vectors.Vector_Bean;
   Root   : constant UBO.Object := UBO.To_Object (List);
begin
   Set := new UBO.Datasets.Dataset;
   Set.Add_Column ("name");
   Set.Add_Column ("email");
   Set.Add_Column ("age");
   for I in 1 .. 100 loop
      Set.Append (Fill_Person'Access);
   end loop;

   List.Append (UBO.To_Object (Set));

   Set := new UBO.Datasets.Dataset;
   Set.Add_Column ("natural");
   Set.Add_Column ("double");
   for I in 1 .. 20 loop
      Set.Append (Fill_Number'Access);
   end loop;
   List.Append (UBO.To_Object (Set));

   declare
      Buffer : aliased Util.Streams.Buffered.Output_Buffer_Stream;
      Print  : aliased Util.Streams.Texts.Print_Stream;
      Output : Util.Serialize.IO.JSON.Output_Stream;
   begin
      Buffer.Initialize (Size => 100000);
      Print.Initialize (Buffer'Unchecked_Access);
      Output.Initialize (Print'Unchecked_Access);

      Output.Write_Entity ("", Root);
      Output.Flush;
      Ada.Text_IO.Put_Line (Util.Streams.Texts.To_String (Buffer));
   end;
end Datasets;
