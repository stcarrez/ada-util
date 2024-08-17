-----------------------------------------------------------------------
--  measures -- Example of Runtime Benchmark
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Gnat.Regexp;
with Gnat.Regpat;
with Util.Measures;
with Util.Strings.Sets;

--
--  This example performs several measures and dumps the result.
--  The result produced by <b>Util.Measures.Write</b> looks like:
--
--  <measures title="Example of measures">
--  <time count="1000" time="406.000000000us" title="Empty"/>
--  <time count="2" time="34.000000000us" title="Ada.Text_IO.Put_Line"/>
--  <time count="1" time="413.000000000us"
--        title="No tracking Empty procedure called 1000 times"/>
--  <time count="1" time="1.960000000ms"
--        title="Tracking Empty procedure called 1000 times"/>
--  </measures>
--
--  'Empty' is called 1000 times for a total time of 406us (or 406ns per call).
procedure Measures is

   procedure Print;
   procedure Empty;

   Perf : Util.Measures.Measure_Set;

   procedure Print is
      S : Util.Measures.Stamp;
   begin
      Ada.Text_IO.Put_Line ("Print test benchmark");
      Util.Measures.Report (Perf, S, "Ada.Text_IO.Put_Line");
   end Print;

   procedure Empty is
      S : Util.Measures.Stamp;
   begin
      Util.Measures.Report (Perf, S, "Empty");
   end Empty;

   procedure Check_Regexp (Name : in String; Found : out Boolean) is
      P : Gnat.Regexp.Regexp := Gnat.Regexp.Compile (".*.o");
   begin
      Found := False;
      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            if Gnat.Regexp.Match (Name, P) then
               Found := True;
            end if;
         end loop;

      Util.Measures.Report (Perf, S,
                            "GNAT.Regexp.Match 1000 times");
      end;
   end Check_Regexp;

   procedure Check_Pattern (Name : in String; Found : out Boolean) is
      P : Gnat.Regpat.Pattern_Matcher := Gnat.Regpat.Compile (".*.o");
   begin
      Found := False;
      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            if Gnat.Regpat.Match (P, Name) then
               Found := True;
            end if;
         end loop;

      Util.Measures.Report (Perf, S,
                            "GNAT.Regpat.Match 1000 times");
      end;
   end Check_Pattern;

   procedure Check_Contains (Name : in String; Found : out Boolean) is
      Set : Util.Strings.Sets.Set;
   begin
      Set.Include ("abdc");
      Set.Include ("bdc");
      Set.Include ("dsdc");
      Set.Include ("dsdsdsdc");
      for I in 1 .. 1_000 loop
         Set.Include ("a" & I'Image);
      end loop;
      Found := False;
      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            if Set.Contains (Name) then
               Found := True;
            end if;
         end loop;

      Util.Measures.Report (Perf, S,
                            "Util.Strings.Sets.Contains 1000 times");
      end;
   end Check_Contains;

   Found : Boolean;
begin
   Print;
   Print;
   --  Measure time for calling 'Empty' 1000 times
   declare
      S : Util.Measures.Stamp;
   begin
      for I in 1 .. 1_000 loop
         Empty;
      end loop;
      Util.Measures.Report (Perf, S,
                            "Tracking Empty procedure called 1000 times");
   end;

   --  Disable measures (the next calls will not be counted)
   Util.Measures.Disable (Perf);
   Print;
   Print;

   --  Measure time for calling 'Empty' 1000 times with performance tracking OFF.
   declare
      S : Util.Measures.Stamp;
   begin
      for I in 1 .. 1_000 loop
         Empty;
      end loop;

      --  Enable measures again to track
      Util.Measures.Enable (Perf);
      Util.Measures.Report (Perf, S,
                            "No tracking Empty procedure called 1000 times");
   end;

   Check_Regexp ("testing.ads", Found);
   Check_Pattern ("testing.ads", Found);
   Check_Contains ("testing.ads", Found);

   --  Dump the result
   Util.Measures.Write (Perf, "Example of measures",
                        Ada.Text_IO.Standard_Output);
end Measures;
