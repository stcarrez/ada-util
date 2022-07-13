-----------------------------------------------------------------------
--  util-streams-buffered -- Buffered streams utilities
--  Copyright (C) 2010 - 2022 Stephane Carrez
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
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Finalization;

--  == Buffered Streams ==
--  The `Output_Buffer_Stream` and `Input_Buffer_Stream` implement an output
--  and input stream respectively which manages an output or input buffer.  The data is
--  first written to the buffer and when the buffer is full or flushed, it gets written
--  to the target output stream.
--
--  The `Output_Buffer_Stream` must be initialized to indicate the buffer size as well
--  as the target output stream onto which the data will be flushed.  For example, a
--  pipe stream could be created and configured to use the buffer as follows:
--
--    with Util.Streams.Buffered;
--    with Util.Streams.Pipes;
--    ...
--       Pipe   : aliased Util.Streams.Pipes.Pipe_Stream;
--       Buffer : Util.Streams.Buffered.Output_Buffer_Stream;
--       ...
--          Buffer.Initialize (Output => Pipe'Unchecked_Access,
--                             Size => 1024);
--
--  In this example, the buffer of 1024 bytes is configured to flush its content to the
--  pipe input stream so that what is written to the buffer will be received as input by
--  the program.
--  The `Output_Buffer_Stream` provides write operation that deal only with binary data
--  (`Stream_Element`).  To write text, it is best to use the `Print_Stream` type from
--  the `Util.Streams.Texts` package as it extends the `Output_Buffer_Stream` and provides
--  several operations to write character and strings.
--
--  The `Input_Buffer_Stream` must also be initialized to also indicate the buffer size
--  and either an input stream or an input content.  When configured, the input stream is used
--  to fill the input stream buffer.  The buffer configuration is very similar as the
--  output stream:
--
--    with Util.Streams.Buffered;
--    with Util.Streams.Pipes;
--    ...
--       Pipe   : aliased Util.Streams.Pipes.Pipe_Stream;
--       Buffer : Util.Streams.Buffered.Input_Buffer_Stream;
--       ...
--          Buffer.Initialize (Input => Pipe'Unchecked_Access, Size => 1024);
--
--  In this case, the buffer of 1024 bytes is filled by reading the pipe stream, and thus
--  getting the program's output.
package Util.Streams.Buffered is

   pragma Preelaborate;

   type Buffer_Access is access Ada.Streams.Stream_Element_Array;

   --  -----------------------
   --  Buffer stream
   --  -----------------------
   --  The `Buffer_Stream` is the base type for the `Output_Buffer_Stream`
   --  and `Input_Buffer_Stream` types.  It holds the buffer and read/write positions.
   type Buffer_Stream is limited new Ada.Finalization.Limited_Controlled with private;

   --  Initialize the stream to read or write on the buffer.
   --  An internal buffer is allocated for writing the stream.
   procedure Initialize (Stream  : in out Buffer_Stream;
                         Size    : in Positive);

   --  Initialize the stream to read from the string.
   procedure Initialize (Stream  : in out Buffer_Stream;
                         Content : in String);

   --  Get the direct access to the buffer.
   function Get_Buffer (Stream : in Buffer_Stream) return Buffer_Access;

   --  Get the number of element in the stream.
   function Get_Size (Stream : in Buffer_Stream) return Natural;

   --  Release the buffer.
   overriding
   procedure Finalize (Object : in out Buffer_Stream);

   --  -----------------------
   --  Output buffer stream
   --  -----------------------
   --  The `Output_Buffer_Stream` is an output stream which uses
   --  an intermediate buffer to write the data.
   --
   --  It is necessary to call `Flush` to make sure the data
   --  is written to the target stream.  The `Flush` operation will
   --  be called when finalizing the output buffer stream.
   type Output_Buffer_Stream is limited new Buffer_Stream and Output_Stream with private;

   --  Initialize the stream to write on the given streams.
   --  An internal buffer is allocated for writing the stream.
   procedure Initialize (Stream  : in out Output_Buffer_Stream;
                         Output  : access Output_Stream'Class;
                         Size    : in Positive);

   --  Initialize the stream with a buffer of <b>Size</b> bytes.
   overriding
   procedure Initialize (Stream  : in out Output_Buffer_Stream;
                         Size    : in Positive);

   --  Close the sink.
   overriding
   procedure Close (Stream : in out Output_Buffer_Stream);

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Output_Buffer_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   overriding
   procedure Flush (Stream : in out Output_Buffer_Stream);

   --  Flush the buffer in the `Into` array and return the index of the
   --  last element (inclusive) in `Last`.
   procedure Flush (Stream : in out Output_Buffer_Stream;
                    Into   : out Ada.Streams.Stream_Element_Array;
                    Last   : out Ada.Streams.Stream_Element_Offset);

   --  Flush the buffer stream to the unbounded string.
   procedure Flush (Stream : in out Output_Buffer_Stream;
                    Into   : out Ada.Strings.Unbounded.Unbounded_String);

   --  Flush the stream and release the buffer.
   overriding
   procedure Finalize (Stream : in out Output_Buffer_Stream);

   type Input_Buffer_Stream is limited new Buffer_Stream and Input_Stream with private;

   --  Initialize the stream to read from the string.
   overriding
   procedure Initialize (Stream  : in out Input_Buffer_Stream;
                         Content : in String);

   --  Initialize the stream to read the given streams.
   procedure Initialize (Stream  : in out Input_Buffer_Stream;
                         Input   : access Input_Stream'Class;
                         Size    : in Positive);

   --  Initialize the stream from the buffer created for an output stream.
   procedure Initialize (Stream  : in out Input_Buffer_Stream;
                         From    : in out Buffer_Stream'Class);

   --  Fill the buffer by reading the input stream.
   --  Raises Data_Error if there is no input stream;
   procedure Fill (Stream : in out Input_Buffer_Stream);

   --  Read one character from the input stream.
   procedure Read (Stream : in out Input_Buffer_Stream;
                   Char   : out Character);

   procedure Read (Stream : in out Input_Buffer_Stream;
                   Value  : out Ada.Streams.Stream_Element);

   procedure Read (Stream : in out Input_Buffer_Stream;
                   Char   : out Wide_Wide_Character);

   --  Read into the buffer as many bytes as possible and return in
   --  `last` the position of the last byte read.
   overriding
   procedure Read (Stream : in out Input_Buffer_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Read (Stream : in out Input_Buffer_Stream;
                   Into   : in out Ada.Strings.Unbounded.Unbounded_String);

   procedure Read (Stream : in out Input_Buffer_Stream;
                   Into   : in out Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);

   --  Returns True if the end of the stream is reached.
   function Is_Eof (Stream : in Input_Buffer_Stream) return Boolean;

   type Input_Output_Buffer_Stream is abstract limited new Buffer_Stream
     and Input_Stream and Output_Stream with private;

private

   use Ada.Streams;

   type Buffer_Stream is limited new Ada.Finalization.Limited_Controlled with record
      --  The buffer where the data is written before being flushed.
      Buffer      : Buffer_Access := null;

      --  The next write position within the buffer.
      Write_Pos   : Stream_Element_Offset := 0;

      --  The next read position within the buffer.
      Read_Pos    : Stream_Element_Offset := 1;

      --  The last valid write position within the buffer.
      Last        : Stream_Element_Offset := 0;
   end record;

   type Output_Buffer_Stream is limited new Buffer_Stream and Output_Stream with record
      --  The output stream to use for flushing the buffer.
      Output      : access Output_Stream'Class;

      No_Flush    : Boolean := False;
   end record;

   type Input_Buffer_Stream is limited new Buffer_Stream and Input_Stream with record
      --  The input stream to use to fill the buffer.
      Input       : access Input_Stream'Class;

      --  Reached end of file when reading.
      Eof         : Boolean := False;
   end record;

   type Input_Output_Buffer_Stream is abstract limited new Buffer_Stream
     and Input_Stream and Output_Stream with record
      --  The output stream to use for flushing the buffer.
      Output      : access Output_Stream'Class;

      No_Flush    : Boolean := False;

      --  The input stream to use to fill the buffer.
      Input       : access Input_Stream'Class;

      --  Reached end of file when reading.
      Eof         : Boolean := False;
   end record;

end Util.Streams.Buffered;
