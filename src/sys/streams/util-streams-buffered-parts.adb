-----------------------------------------------------------------------
--  util-streams-buffered-parts -- Buffered stream which stops on a boundary
--  Copyright (C) 2023 Stephane Carrez
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
with Ada.Unchecked_Deallocation;

package body Util.Streams.Buffered.Parts is

   procedure Free_Buffer is
     new Ada.Unchecked_Deallocation (Object => Stream_Element_Array,
                                     Name   => Buffer_Access);

   --  ------------------------------
   --  Initialize the stream to read from the string.
   --  ------------------------------
   overriding
   procedure Initialize (Stream  : in out Input_Part_Stream;
                         Content : in String) is
   begin
      Input_Buffer_Stream (Stream).Initialize (Content);
      Stream.Real_Write_Pos := 1;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream to read the given input stream.
   --  ------------------------------
   overriding
   procedure Initialize (Stream  : in out Input_Part_Stream;
                         Input   : access Input_Stream'Class;
                         Size    : in Positive) is
   begin
      Input_Buffer_Stream (Stream).Initialize (Input, Size);
      Stream.Real_Write_Pos := 1;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream from the buffer created for an output stream.
   --  ------------------------------
   overriding
   procedure Initialize (Stream  : in out Input_Part_Stream;
                         From    : in out Buffer_Stream'Class) is
   begin
      Input_Buffer_Stream (Stream).Initialize (From);
      Stream.Real_Write_Pos := 1;
   end Initialize;

   --  ------------------------------
   --  Set the boundary when reading the input stream.
   --  ------------------------------
   procedure Set_Boundary (Stream   : in out Input_Part_Stream;
                           Boundary : in Ada.Streams.Stream_Element_Array) is
   begin
      if Stream.Boundary /= null then
         Free_Buffer (Stream.Boundary);
      end if;
      Stream.Boundary := new Ada.Streams.Stream_Element_Array '(Boundary);
      Stream.Next_Part;
   end Set_Boundary;

   procedure Set_Boundary (Stream   : in out Input_Part_Stream;
                           Boundary : in String) is
      Content : Stream_Element_Array
        (Stream_Element_Offset (Boundary'First) .. Stream_Element_Offset (Boundary'Last));
      for Content'Address use Boundary'Address;
   begin
      Stream.Set_Boundary (Content);
   end Set_Boundary;

   --  ------------------------------
   --  Prepare to read the next part with the same boundary.
   --  ------------------------------
   procedure Next_Part (Stream : in out Input_Part_Stream) is
   begin
      Stream.Eob := False;
      if Stream.Real_Write_Pos > Stream.Write_Pos then
         Stream.Write_Pos := Stream.Real_Write_Pos;
      end if;
      if Stream.Read_Pos < Stream.Write_Pos then
         Stream.Check_Boundary;
      end if;
   end Next_Part;

   overriding
   procedure Fill (Stream : in out Input_Part_Stream) is
      Pos      : Stream_Element_Offset;
      Boundary : constant Buffer_Access := Stream.Boundary;
      Buffer   : constant Buffer_Access := Stream.Buffer;
   begin
      if Stream.Eof or else Stream.Eob then
         return;
      end if;
      if Stream.Input = null then
         Stream.Eof := True;
         return;
      end if;

      --  Buffer area (Write_Pos .. Real_Write_Pos) contains a possible boundary marker.
      --  Move it at beginning and compare it for a match.
      if Stream.Real_Write_Pos > Stream.Write_Pos then
         Pos := Stream.Real_Write_Pos - Stream.Write_Pos;
         Buffer (1 .. Pos) := Buffer (Stream.Write_Pos .. Stream.Real_Write_Pos - 1);
         if Pos >= Boundary'Length and then Buffer (1 .. Boundary'Length) = Boundary.all then
            Stream.Eob := True;
            Stream.Write_Pos := Boundary'Length + 1;
            Stream.Read_Pos := Boundary'Length + 1;
            Stream.Real_Write_Pos := Pos + 1;
            return;
         end if;

         Stream.Input.Read (Stream.Buffer (Pos + 1 .. Stream.Last), Stream.Write_Pos);
         Stream.Eof := Stream.Write_Pos < Pos + 1;
      else
         Stream.Real_Write_Pos := 1;
         Stream.Input.Read (Stream.Buffer (1 .. Stream.Last), Stream.Write_Pos);
         Stream.Eof := Stream.Write_Pos < 1;
      end if;

      if not Stream.Eof then
         Stream.Write_Pos := Stream.Write_Pos + 1;
         Stream.Read_Pos := 1;
      end if;
      Stream.Check_Boundary;
   end Fill;

   --  ------------------------------
   --  Check if the current buffer contains some possible boundary marker.
   --  Update `Write_Pos` to limit the `Read` operation and force a call
   --  to `Fill` that will check for final complete boundary check and
   --  mark the buffer with `eob=true`.
   --  ------------------------------
   procedure Check_Boundary (Stream : in out Input_Part_Stream) is
      Boundary : constant Buffer_Access := Stream.Boundary;
      Buffer   : constant Buffer_Access := Stream.Buffer;
      C        : constant Stream_Element := Boundary (1);
      Pos      : Stream_Element_Offset := Stream.Read_Pos;
      Count    : Stream_Element_Offset;
   begin
      while Pos < Stream.Write_Pos loop
         if Buffer (Pos) = C then
            if Pos + Boundary'Length <= Stream.Write_Pos then
               Count := Boundary'Length;
            else
               Count := Stream.Write_Pos - Pos;
            end if;
            if Buffer (Pos .. Pos + Count - 1) = Boundary (1 .. Count) then
               if Stream.Real_Write_Pos < Stream.Write_Pos then
                  Stream.Real_Write_Pos := Stream.Write_Pos;
               end if;
               if Pos = Stream.Read_Pos and then Count = Boundary'Length then
                  Pos := Pos + Count;
                  Stream.Read_Pos := Pos;
                  Stream.Eob := True;
               end if;
               Stream.Write_Pos := Pos;
               exit;
            end if;
         end if;
         Pos := Pos + 1;
      end loop;
   end Check_Boundary;

   --  ------------------------------
   --  Returns True if the end of the boundary is reached.
   --  ------------------------------
   function Is_Eob (Stream : in out Input_Part_Stream) return Boolean is
   begin
      if Stream.Eob or else Stream.Eof then
         return True;
      end if;

      --  Done if we have not reached the end of buffer or it does not contain a possible boundary.
      if Stream.Read_Pos < Stream.Write_Pos or else Stream.Real_Write_Pos < Stream.Write_Pos then
         return False;
      end if;

      --  We could have reached a boundary.
      declare
         Pos      : Stream_Element_Offset := Stream.Read_Pos;
         Boundary : constant Buffer_Access := Stream.Boundary;
         Buffer   : constant Buffer_Access := Stream.Buffer;
      begin
         if Pos + Boundary'Length <= Stream.Real_Write_Pos then
            if Buffer (Pos .. Pos + Boundary'Length - 1) = Boundary.all then
               Pos := Pos + Boundary'Length;
               Stream.Read_Pos := Pos;
               Stream.Eob := True;
               Stream.Write_Pos := Pos;
               return True;
            end if;
            return False;
         else
            --  Not enough data to check, fill the buffer and check.
            --  We could reach eof but this is not yet the eob because we still have unread data.
            Stream.Fill;
            return Stream.Eob;
         end if;
      end;
   end Is_Eob;

   --  ------------------------------
   --  Release the buffer.
   --  ------------------------------
   overriding
   procedure Finalize (Stream : in out Input_Part_Stream) is
   begin
      if Stream.Boundary /= null then
         Free_Buffer (Stream.Boundary);
      end if;
      Input_Buffer_Stream (Stream).Finalize;
   end Finalize;

end Util.Streams.Buffered.Parts;
