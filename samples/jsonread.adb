with Ada.Text_IO;
with Ada.Command_Line;
with Util.Serialize.IO.JSON;
with Util.Beans.Objects;
with Util.Beans.Objects.Iterators;
procedure Jsonread is

   package UBO renames Util.Beans.Objects;
   procedure Dump (Object : in UBO.Object;
                   Indent : in Natural);

   procedure Dump (Object : in UBO.Object;
                   Indent : in Natural) is
      Iter : UBO.Iterators.Iterator := UBO.Iterators.First (Object);
      Pos  : Natural := 0;
   begin
      while UBO.Iterators.Has_Element (Iter) loop
         Ada.Text_IO.Set_Col (Ada.Text_IO.Count (Indent + 1));
         if UBO.Iterators.Has_Key (Iter) then
            Ada.Text_IO.Put (UBO.Iterators.Key (Iter) & "=");
         else
            Pos := Pos + 1;
            Ada.Text_IO.Put (Positive'Image (Pos) & "=");
         end if;
         declare
            Item : constant UBO.Object := UBO.Iterators.Element (Iter);
         begin
            if UBO.To_Bean (Item) /= null then
               Dump (Item, Indent + 1);
            else
               Ada.Text_IO.Put_Line (UBO.To_String (Item));
            end if;
         end;
         UBO.Iterators.Next (Iter);
      end loop;
   end Dump;

   Root   : UBO.Object;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: jsonread path");
      return;
   end if;

   for I in 1 .. Ada.Command_Line.Argument_Count loop
      Root := Util.Serialize.IO.JSON.Read (Ada.Command_Line.Argument (I));
      Dump (Root, 0);
   end loop;

end Jsonread;
