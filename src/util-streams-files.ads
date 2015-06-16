-----------------------------------------------------------------------
--  Util.Streams.Files -- File Stream utilities
--  Copyright (C) 2010, 2013, 2015 Stephane Carrez
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
with Ada.Streams.Stream_IO;
package Util.Streams.Files is

   --  -----------------------
   --  File stream
   --  -----------------------
   --  The <b>File_Stream</b> is an output/input stream that reads or writes
   --  into a file-based stream.
   type File_Stream is limited new Output_Stream and Input_Stream with private;

   --  Open the file and initialize the stream for reading or writing.
   procedure Open (Stream  : in out File_Stream;
                   Mode    : in Ada.Streams.Stream_IO.File_Mode;
                   Name    : in String := "";
                   Form    : in String := "");

   --  Create the file and initialize the stream for writing.
   procedure Create (Stream  : in out File_Stream;
                     Mode    : in Ada.Streams.Stream_IO.File_Mode;
                     Name    : in String := "";
                     Form    : in String := "");

   --  Close the stream.
   overriding
   procedure Close (Stream : in out File_Stream);

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out File_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   overriding
   procedure Read (Stream : in out File_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

private

   use Ada.Streams;

   type File_Stream is limited new Ada.Finalization.Limited_Controlled
     and Output_Stream and Input_Stream with record
      File : Ada.Streams.Stream_IO.File_Type;
   end record;

   --  Flush the stream and release the buffer.
   overriding
   procedure Finalize (Object : in out File_Stream);

end Util.Streams.Files;
