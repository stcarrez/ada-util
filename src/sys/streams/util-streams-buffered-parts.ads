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

--  == Part streams ==
--  The `Input_Part_Stream` is an input stream which decomposes an input stream
--  in several parts separated by well known and fixed boundaries.
package Util.Streams.Buffered.Parts is

   type Input_Part_Stream is limited new Input_Buffer_Stream with private;

   --  Initialize the stream to read from the string.
   overriding
   procedure Initialize (Stream  : in out Input_Part_Stream;
                         Content : in String);

   --  Initialize the stream to read the given input stream.
   overriding
   procedure Initialize (Stream  : in out Input_Part_Stream;
                         Input   : access Input_Stream'Class;
                         Size    : in Positive);

   --  Initialize the stream from the buffer created for an output stream.
   overriding
   procedure Initialize (Stream  : in out Input_Part_Stream;
                         From    : in out Buffer_Stream'Class);

   --  Set the boundary when reading the input stream.
   procedure Set_Boundary (Stream   : in out Input_Part_Stream;
                           Boundary : in Ada.Streams.Stream_Element_Array) with
      Pre => Boundary'Length < Stream.Get_Buffer'Length;
   procedure Set_Boundary (Stream   : in out Input_Part_Stream;
                           Boundary : in String);

   --  Prepare to read the next part with the same boundary.
   procedure Next_Part (Stream : in out Input_Part_Stream) with
      Pre => Stream.Is_Eob;

   --  Fill the buffer by reading the input stream.
   --  Raises Data_Error if there is no input stream;
   overriding
   procedure Fill (Stream : in out Input_Part_Stream);

   --  Returns True if the end of the boundary is reached.
   function Is_Eob (Stream : in out Input_Part_Stream) return Boolean;

   --  Release the buffer.
   overriding
   procedure Finalize (Stream : in out Input_Part_Stream);

private

   type Input_Part_Stream is limited new Input_Buffer_Stream with record
      --  The next read position within the buffer.
      Real_Write_Pos : Stream_Element_Offset := 1;

      --  The boundary content that serves as delimiter.
      Boundary    : Buffer_Access;
   end record;

   --  Check if the current buffer contains some possible boundary marker.
   --  Update `Write_Pos` to limit the `Read` operation and force a call
   --  to `Fill` that will check for final complete boundary check and
   --  mark the buffer with `eob=true`.
   procedure Check_Boundary (Stream : in out Input_Part_Stream);

end Util.Streams.Buffered.Parts;
