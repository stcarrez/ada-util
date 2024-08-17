with Ada.Text_IO;
with Ada.Command_Line;
with Util.Properties;

procedure Proplist is

   procedure Print (Name  : in String;
                    Value : in Util.Properties.Value);

   procedure Print (Name  : in String;
                    Value : in Util.Properties.Value) is
   begin
      if Util.Properties.Is_Manager (Value) then
         Ada.Text_IO.Put_Line ("[" & Name & "]");
         Util.Properties.To_Manager (Value).Iterate (Print'Access);
         Ada.Text_IO.New_Line;
      else
         Ada.Text_IO.Put (Name);
         Ada.Text_IO.Put ("=");
         Ada.Text_IO.Put_Line (Util.Properties.To_String (Value));
      end if;
   end Print;

   Properties : Util.Properties.Manager;
   Count : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: proplist {file.properties | file.ini}...");
      Ada.Text_IO.Put_Line ("Example: proplist ./samples/test.properties ./samples/test.ini");
      return;
   end if;
   for I in 1 .. Count loop
      Properties.Load_Properties (Path => Ada.Command_Line.Argument (I));

      Properties.Iterate (Print'Access);
   end loop;
end Proplist;
