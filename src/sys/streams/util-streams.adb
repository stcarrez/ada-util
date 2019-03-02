-----------------------------------------------------------------------
--  util-streams -- Stream utilities
--  Copyright (C) 2010, 2011, 2016, 2018 Stephane Carrez
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

package body Util.Streams is

   use Ada.Streams;

   --  ------------------------------
   --  Copy the input stream to the output stream until the end of the input stream
   --  is reached.
   --  ------------------------------
   procedure Copy (From : in out Input_Stream'Class;
                   Into : in out Output_Stream'Class) is
      Buffer : Stream_Element_Array (0 .. 4_096);
      Last   : Stream_Element_Offset;
   begin
      loop
         From.Read (Buffer, Last);
         if Last > Buffer'First then
            Into.Write (Buffer (Buffer'First .. Last));
         end if;
         exit when Last < Buffer'Last;
      end loop;
   end Copy;

   --  ------------------------------
   --  Copy the stream array to the string.
   --  The string must be large enough to hold the stream array
   --  or a Constraint_Error exception is raised.
   --  ------------------------------
   procedure Copy (From : in Ada.Streams.Stream_Element_Array;
                   Into : in out String) is
      Pos : Positive := Into'First;
   begin
      for I in From'Range loop
         Into (Pos) := Character'Val (From (I));
         Pos := Pos + 1;
      end loop;
   end Copy;

   --  ------------------------------
   --  Copy the string to the stream array.
   --  The stream array must be large enough to hold the string
   --  or a Constraint_Error exception is raised.
   --  ------------------------------
   procedure Copy (From : in String;
                   Into : in out Ada.Streams.Stream_Element_Array) is
      Pos : Ada.Streams.Stream_Element_Offset := Into'First;
   begin
      for I in From'Range loop
         Into (Pos) := Character'Pos (From (I));
         Pos := Pos + 1;
      end loop;
   end Copy;

end Util.Streams;
