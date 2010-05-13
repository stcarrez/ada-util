-----------------------------------------------------------------------
--  measure -- Benchmark tools
--  Copyright (C) 2008, 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Task_Attributes;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;
package body Util.Measures is

   procedure Free is
     new Ada.Unchecked_Deallocation (Buckets_Type, Buckets_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Measure, Measure_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (String, String_Access);

   package Task_Context is new Ada.Task_Attributes
     (Measure_Set_Access, null);

   function Format (D : Duration) return String;

   --  ------------------------------
   --  Disable collecting measures on the measure set.
   --  ------------------------------
   procedure Disable (Measures : in out Measure_Set) is
   begin
      Measures.Enabled := False;
   end Disable;

   --  ------------------------------
   --  Enable collecting measures on the measure set.
   --  ------------------------------
   procedure Enable (Measures : in out Measure_Set) is
   begin
      Measures.Enabled := True;
   end Enable;

   --  ------------------------------
   --  Set the per-thread measure set.
   --  ------------------------------
   procedure Set_Current (Measures : in Measure_Set_Access) is
   begin
      Task_Context.Set_Value (Measures);
   end Set_Current;

   --  ------------------------------
   --  Get the per-thread measure set.
   --  ------------------------------
   function Get_Current return Measure_Set_Access is
   begin
      return Task_Context.Value;
   end Get_Current;

   --  ------------------------------
   --  Dump an XML result with the measures collected by the measure set.
   --  ------------------------------
   procedure Write (Measures : in out Measure_Set;
                    Title    : in String;
                    Stream   : in Ada.Text_IO.File_Type) is
      use Ada.Text_IO;

      procedure Dump_XML (Item : in Measure_Access) is
         Count : constant String := Positive'Image (Item.Count);
         Time  : constant String := Format (Item.Time);
      begin
         Put (Stream, "<time count=""");
         Put (Stream, Count (Count'First + 1 .. Count'Last));
         Put (Stream, """ time=""");
         Put (Stream, Time (Time'First + 1 .. Time'Last));
         Put (Stream, """ title=""");
         Put (Stream, Item.Name.all);
         Put_Line (Stream, """/>");
      end Dump_XML;

      Buckets : Buckets_Access;

   begin
      Put (Stream, "<measures title=""");
      Put (Stream, title);
      Put_Line (Stream, """>");
      Measures.Data.Steal_Map (Buckets);
      if Buckets /= null then
         for I in Buckets'Range loop
            declare
               Next : Measure_Access;
               Node : Measure_Access := Buckets (I);
            begin
               while Node /= null loop
                  Dump_XML (Node);
                  Free (Node.Name);
                  Next := Node.Next;
                  Free (Node);
                  Node := Next;
               end loop;
            end;
         end loop;
         Free (Buckets);
      end if;
      Put_Line (Stream, "</measures>");
   end Write;

   --  ------------------------------
   --  Report the time spent between the stamp creation and this method call.
   --  Collect the result in the per-thread measure set under the given measure
   --  title.
   --  ------------------------------
   procedure Report (S     : in out Stamp;
                     Title : in String) is
      Measures : constant Measure_Set_Access := Task_Context.Value;
   begin
      if Measures /= null and then Measures.Enabled then
         Report (Measures.all, S, Title);
      end if;
   end Report;

   --  ------------------------------
   --  Report the time spent between the stamp creation and this method call.
   --  Collect the result in the measure set under the given measure title.
   --  ------------------------------
   procedure Report (Measures : in out Measure_Set;
                     S        : in out Stamp;
                     Title    : in String) is
      use Ada.Calendar;
   begin
      if Measures.Enabled then
         declare
            D : constant Duration := Ada.Calendar.Clock - S.Start;
         begin
            Measures.Data.Add (Title, D);
         end;
         S.Start := Ada.Calendar.Clock;
      end if;
   end Report;

   protected body Measure_Data is

      --  ------------------------------
      --  Get the measures and clear to start a new set of measures.
      --  ------------------------------
      entry Steal_Map (Result : out Buckets_Access) when True is
      begin
         Result  := Buckets;
         Buckets := null;
      end Steal_Map;

      --  ------------------------------
      --  Add the measure
      --  ------------------------------
      entry Add (Title : String; D : Duration) when True is

         use Ada.Containers;
         use Ada.Calendar;

         Pos  : Hash_Type;
         Node : Measure_Access;
      begin
         if Buckets = null then
            Buckets := new Buckets_Type (0 .. 256);
         end if;
         Pos := Ada.Strings.Hash (Title) mod Buckets'Length;
         Node := Buckets (Pos);
         while Node /= null loop
            if Node.Name'Length = Title'Length and then Node.Name.all = Title then
               Node.Count := Node.Count + 1;
               Node.Time := Node.Time + D;
               return;
            end if;
            Node := Node.Next;
         end loop;
         Buckets (Pos) := new Measure '(Name => new String '(Title),
                                        Time  => D,
                                        Count => 1,
                                        Next  => Buckets (Pos));
      end Add;

   end Measure_Data;

   --  ------------------------------
   --  Format the duration in a time in 'ns', 'us', 'ms' or seconds.
   --  ------------------------------
   function Format (D : Duration) return String is
   begin
      if D < 0.000_001 then
         return Duration'Image (D * 1_000_000_000) & "ns";
      elsif D < 0.001 then
         return Duration'Image (D * 1_000_000) & "us";
      elsif D < 1.0 then
         return Duration'Image (D * 1_000) & "ms";
      else
         return Duration'Image (D) & "s";
      end if;
   end Format;

end Util.Measures;
