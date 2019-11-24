-----------------------------------------------------------------------
--  util-streams-raw -- Raw streams for Windows based systems
--  Copyright (C) 2011, 2019 Stephane Carrez
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
with Ada.Finalization;

with Util.Systems.Os;
with Util.Systems.Types;

--  The <b>Util.Streams.Raw</b> package provides a stream directly on top of
--  file system operations <b>ReadFile</b> and <b>WriteFile</b>.
package Util.Streams.Raw is

   subtype File_Type is Util.Systems.Os.File_Type;

   --  -----------------------
   --  File stream
   --  -----------------------
   --  The <b>Raw_Stream</b> is an output/input stream that reads or writes
   --  into a file-based stream.
   type Raw_Stream is new Ada.Finalization.Limited_Controlled and Output_Stream
     and Input_Stream with private;
   type Raw_Stream_Access is access all Raw_Stream'Class;

   --  Initialize the raw stream to read and write on the given file descriptor.
   procedure Initialize (Stream  : in out Raw_Stream;
                         File    : in File_Type);

   --  Get the file descriptor associated with the stream.
   function Get_File (Stream : in Raw_Stream) return Util.Systems.Os.File_Type;

   --  Set the file descriptor to be used by the stream.
   procedure Set_File (Stream : in out Raw_Stream;
                       File   : in Util.Systems.Os.File_Type);

   --  Close the stream.
   overriding
   procedure Close (Stream : in out Raw_Stream);

   --  Write the buffer array to the output stream.
   procedure Write (Stream : in out Raw_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   procedure Read (Stream : in out Raw_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

   --  Reposition the read/write file offset.
   procedure Seek (Stream : in out Raw_Stream;
                   Pos    : in Util.Systems.Types.off_t;
                   Mode   : in Util.Systems.Types.Seek_Mode);

private

   use Ada.Streams;

   --  Flush the stream and release the buffer.
   overriding
   procedure Finalize (Object : in out Raw_Stream);

   type Raw_Stream is new Ada.Finalization.Limited_Controlled
     and Output_Stream and Input_Stream with record
      File : File_Type := Util.Systems.Os.NO_FILE;
   end record;

end Util.Streams.Raw;
