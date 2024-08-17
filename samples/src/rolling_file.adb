with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;
with Util.Files.Rolling;
with Util.Dates.ISO8601;
procedure Rolling_File is
   Count   : constant Natural := Ada.Command_Line.Argument_Count;
   Content : constant String := Util.Dates.ISO8601.Image (Ada.Calendar.Clock,
                                                          Util.Dates.ISO8601.SECOND);
   Manager : Util.Files.Rolling.File_Manager;
begin
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: rolling_file {ascending|descending|direct}");
      return;
   end if;

   declare
      Mode : constant String := Ada.Command_Line.Argument (1);
   begin
      if Mode = "ascending" then
         Manager.Initialize ("rolling.log", "rolling-%i.log",
                             Policy   => (Kind => Util.Files.Rolling.No_Policy),
                             Strategy => (Util.Files.Rolling.Ascending_Strategy, 1, 5));
      elsif Mode = "descending" then
         Manager.Initialize ("rolling.log", "rolling-%i.log",
                             Policy   => (Kind => Util.Files.Rolling.No_Policy),
                             Strategy => (Util.Files.Rolling.Descending_Strategy, 1, 5));
      elsif Mode = "direct" then
         Manager.Initialize ("rolling.log", "rolling-%i.log",
                             Policy   => (Kind => Util.Files.Rolling.No_Policy),
                             Strategy => (Util.Files.Rolling.Direct_Strategy, 5));
      else
         Ada.Text_IO.Put_Line ("Invalid mode: " & Mode);
         return;
      end if;
   end;
   Util.Files.Write_File (Manager.Get_Current_Path,
                          Content & ASCII.LF);
end Rolling_File;
