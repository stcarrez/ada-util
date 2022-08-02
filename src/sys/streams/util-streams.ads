-----------------------------------------------------------------------
--  util-streams -- Stream utilities
--  Copyright (C) 2010, 2016, 2018, 2019, 2020, 2022 Stephane Carrez
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

with Ada.Streams;

--  = Streams =
--  The `Util.Streams` package provides several types and operations to allow the
--  composition of input and output streams.  Input streams can be chained together so that
--  they traverse the different stream objects when the data is read from them.  Similarly,
--  output streams can be chained and the data that is written will traverse the different
--  streams from the first one up to the last one in the chain.  During such traversal, the
--  stream object is able to bufferize the data or make transformations on the data.
--
--  The `Input_Stream` interface represents the stream to read data.  It only provides a
--  `Read` procedure.  The `Output_Stream` interface represents the stream to write data.
--  It provides a `Write`, `Flush` and `Close` operation.
--
--  To use the packages described here, use the following GNAT project:
--
--    with "utilada_sys";
--
--  @include util-streams-buffered.ads
--  @include util-streams-texts.ads
--  @include util-streams-files.ads
--  @include util-streams-pipes.ads
--  @include util-streams-sockets.ads
--  @include util-streams-raw.ads
--  @include util-streams-buffered-encoders.ads
--  @include util-streams-base16.ads
--  @include util-streams-base64.ads
--  @include util-streams-aes.ads
package Util.Streams is

   pragma Preelaborate;

   --  -----------------------
   --  Output stream
   --  -----------------------
   --  The <b>Output_Stream</b> is an interface that accepts output bytes
   --  and sends them to a sink.
   type Output_Stream is limited interface;
   type Output_Stream_Access is access all Output_Stream'Class;

   --  Write the buffer array to the output stream.
   procedure Write (Stream : in out Output_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is abstract;

   --  Flush the buffer (if any) to the sink.
   procedure Flush (Stream : in out Output_Stream) is null;

   --  Close the sink.
   procedure Close (Stream : in out Output_Stream) is null;

   --  -----------------------
   --  Input stream
   --  -----------------------
   --  The <b>Input_Stream</b> is the interface that reads input bytes
   --  from a source and returns them.
   type Input_Stream is limited interface;
   type Input_Stream_Access is access all Input_Stream'Class;

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   procedure Read (Stream : in out Input_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is abstract;

   --  Copy the input stream to the output stream until the end of the input stream
   --  is reached.
   procedure Copy (From : in out Input_Stream'Class;
                   Into : in out Output_Stream'Class);

   --  Copy the stream array to the string.
   --  The string must be large enough to hold the stream array
   --  or a Constraint_Error exception is raised.
   procedure Copy (From : in Ada.Streams.Stream_Element_Array;
                   Into : in out String);

   --  Copy the string to the stream array.
   --  The stream array must be large enough to hold the string
   --  or a Constraint_Error exception is raised.
   procedure Copy (From : in String;
                   Into : in out Ada.Streams.Stream_Element_Array);

   --  Write a raw string on the stream.
   procedure Write (Stream : in out Output_Stream'Class;
                    Item   : in Character);

   procedure Write (Stream : in out Output_Stream'Class;
                    Item   : in String);

   --  Write a wide character on the stream using a UTF-8 sequence.
   procedure Write_Wide (Stream : in out Output_Stream'Class;
                         Item   : in Wide_Wide_Character);

   procedure Write_Wide (Stream : in out Output_Stream'Class;
                         Item   : in Wide_Wide_String);

   --  Notes:
   --  ------
   --  The <b>Ada.Streams.Root_Stream_Type</b> implements the <b>Output_Stream</b>
   --  and <b>Input_Stream</b>.  It is however not easy to use for composing various
   --  stream behaviors.
end Util.Streams;
