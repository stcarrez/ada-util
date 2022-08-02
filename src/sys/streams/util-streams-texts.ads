-----------------------------------------------------------------------
--  util-streams-texts -- Text stream utilities
--  Copyright (C) 2010, 2011, 2012, 2015, 2016, 2017, 2018, 2019, 2020, 2022 Stephane Carrez
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
with Util.Streams.Buffered;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;

--  == Texts ==
--  The `Util.Streams.Texts` package implements text oriented input and output streams.
--  The `Print_Stream` type extends the `Output_Buffer_Stream` to allow writing
--  text content.
--
--  The `Reader_Stream` type extends the `Input_Buffer_Stream` and allows to
--  read text content.
package Util.Streams.Texts is

   --  -----------------------
   --  Print stream
   --  -----------------------
   --  The <b>Print_Stream</b> is an output stream which provides helper methods
   --  for writing text streams.
   type Print_Stream is new Buffered.Output_Buffer_Stream with private;
   type Print_Stream_Access is access all Print_Stream'Class;

   procedure Initialize (Stream : in out Print_Stream;
                         To     : access Output_Stream'Class);

   --  Write a raw string on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String);

   --  Write a raw string on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);

   --  Write an integer on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Integer);

   --  Write an integer on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Long_Long_Integer);

   --  Write a date on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Calendar.Time;
                    Format : in GNAT.Calendar.Time_IO.Picture_String
                    := GNAT.Calendar.Time_IO.ISO_Date);

   --  Get the output stream content as a string.
   function To_String (Stream : in Buffered.Output_Buffer_Stream'Class) return String;

   --  Write a character on the stream.
   procedure Write_Char (Stream : in out Print_Stream'Class;
                         Item   : in Character);

   --  Write a character on the stream.
   procedure Write_Char (Stream : in out Print_Stream'Class;
                         Item   : in Wide_Wide_Character);

   --  -----------------------
   --  Reader stream
   --  -----------------------
   --  The <b>Reader_Stream</b> is an input stream which provides helper methods
   --  for reading text streams.
   type Reader_Stream is new Buffered.Input_Buffer_Stream with private;
   type Reader_Stream_Access is access all Reader_Stream'Class;

   --  Initialize the reader to read the input from the input stream given in <b>From</b>.
   procedure Initialize (Stream : in out Reader_Stream;
                         From   : access Input_Stream'Class);

   --  Read an input line from the input stream.  The line is terminated by ASCII.LF.
   --  When <b>Strip</b> is set, the line terminators (ASCII.CR, ASCII.LF) are removed.
   procedure Read_Line (Stream : in out Reader_Stream;
                        Into   : out Ada.Strings.Unbounded.Unbounded_String;
                        Strip  : in Boolean := False);

private

   type Print_Stream is new Buffered.Output_Buffer_Stream with null record;

   type Reader_Stream is new Buffered.Input_Buffer_Stream with null record;

end Util.Streams.Texts;
