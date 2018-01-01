-----------------------------------------------------------------------
--  util-streams-buffered -- Buffered streams utilities
--  Copyright (C) 2010, 2013, 2015, 2016, 2017, 2018 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Finalization;

--  === Buffered Streams ===
--  The <tt>Output_Buffer_Stream</tt> and <tt>Input_Buffer_Stream</tt> implements an output
--  and input stream respectively which manages an output or input buffer.
--
--  The <tt>Output_Buffer_Stream</tt> must be initialized to indicate the buffer size as well
--  as the target output stream onto which the data will be flushed.
--
--  The <tt>Input_Buffer_Stream</tt> must also be initialized to also indicate the buffer size
--  and either an input stream or an input content.  When configured, the input stream is used
--  to fill the input stream buffer.
package Util.Streams.Buffered is

   pragma Preelaborate;

   type Buffer_Access is access Ada.Streams.Stream_Element_Array;

   --  -----------------------
   --  Output buffer stream
   --  -----------------------
   --  The <b>Output_Buffer_Stream</b> is an output stream which uses
   --  an intermediate buffer to write the data.
   --
   --  It is necessary to call <b>Flush</b> to make sure the data
   --  is written to the target stream.  The <b>Flush</b> operation will
   --  be called when finalizing the output buffer stream.
   type Output_Buffer_Stream is limited new Output_Stream with private;

   --  Initialize the stream to write on the given streams.
   --  An internal buffer is allocated for writing the stream.
   procedure Initialize (Stream  : in out Output_Buffer_Stream;
                         Output  : in Output_Stream_Access;
                         Size    : in Positive);

   --  Initialize the stream with a buffer of <b>Size</b> bytes.
   procedure Initialize (Stream  : in out Output_Buffer_Stream;
                         Size    : in Positive);

   --  Close the sink.
   overriding
   procedure Close (Stream : in out Output_Buffer_Stream);

   --  Get the direct access to the buffer.
   function Get_Buffer (Stream : in Output_Buffer_Stream) return Buffer_Access;

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Output_Buffer_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   overriding
   procedure Flush (Stream : in out Output_Buffer_Stream);

   --  Flush the buffer in the <tt>Into</tt> array and return the index of the
   --  last element (inclusive) in <tt>Last</tt>.
   procedure Flush (Stream : in out Output_Buffer_Stream;
                    Into   : out Ada.Streams.Stream_Element_Array;
                    Last   : out Ada.Streams.Stream_Element_Offset);

   --  Flush the buffer stream to the unbounded string.
   procedure Flush (Stream : in out Output_Buffer_Stream;
                    Into   : out Ada.Strings.Unbounded.Unbounded_String);

   --  Get the number of element in the stream.
   function Get_Size (Stream : in Output_Buffer_Stream) return Natural;

   type Input_Buffer_Stream is limited new Input_Stream with private;

   --  Initialize the stream to read from the string.
   procedure Initialize (Stream  : in out Input_Buffer_Stream;
                         Content : in String);

   --  Initialize the stream to read the given streams.
   procedure Initialize (Stream  : in out Input_Buffer_Stream;
                         Input   : in Input_Stream_Access;
                         Size    : in Positive);

   --  Fill the buffer by reading the input stream.
   --  Raises Data_Error if there is no input stream;
   procedure Fill (Stream : in out Input_Buffer_Stream);

   --  Read one character from the input stream.
   procedure Read (Stream : in out Input_Buffer_Stream;
                   Char   : out Character);

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   overriding
   procedure Read (Stream : in out Input_Buffer_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   procedure Read (Stream : in out Input_Buffer_Stream;
                   Into   : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Returns True if the end of the stream is reached.
   function Is_Eof (Stream : in Input_Buffer_Stream) return Boolean;

private

   use Ada.Streams;

   type Output_Buffer_Stream is limited new Ada.Finalization.Limited_Controlled
     and Output_Stream with record
      --  The buffer where the data is written before being flushed.
      Buffer      : Buffer_Access := null;

      --  The next write position within the buffer.
      Write_Pos   : Stream_Element_Offset := 0;

      --  The next read position within the buffer.
      Read_Pos    : Stream_Element_Offset := 1;

      --  The last valid write position within the buffer.
      Last        : Stream_Element_Offset := 0;

      --  The output stream to use for flushing the buffer.
      Output      : Output_Stream_Access := null;

      No_Flush    : Boolean := False;
   end record;

   --  Flush the stream and release the buffer.
   overriding
   procedure Finalize (Object : in out Output_Buffer_Stream);

   type Input_Buffer_Stream is limited new Ada.Finalization.Limited_Controlled
     and Input_Stream with record
      --  The buffer where the data is written before being flushed.
      Buffer      : Buffer_Access := null;

      --  The next write position within the buffer.
      Write_Pos   : Stream_Element_Offset := 0;

      --  The next read position within the buffer.
      Read_Pos    : Stream_Element_Offset := 1;

      --  The last valid write position within the buffer.
      Last        : Stream_Element_Offset := 0;

      --  The input stream to use to fill the buffer.
      Input       : Input_Stream_Access := null;

      --  Reached end of file when reading.
      Eof         : Boolean := False;
   end record;

   --  Release the buffer.
   overriding
   procedure Finalize (Object : in out Input_Buffer_Stream);

end Util.Streams.Buffered;
