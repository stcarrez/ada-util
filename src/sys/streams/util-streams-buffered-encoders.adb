-----------------------------------------------------------------------
--  util-streams-encoders -- Streams with encoding and decoding capabilities
--  Copyright (C) 2017, 2018, 2019, 2021, 2022 Stephane Carrez
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
package body Util.Streams.Buffered.Encoders is

   --  -----------------------
   --  Initialize the stream with a buffer of <b>Size</b> bytes.
   --  -----------------------
   overriding
   procedure Initialize (Stream  : in out Encoder_Stream;
                         Size    : in Positive) is
   begin
      Buffer_Stream (Stream).Initialize (Size);
      Stream.No_Flush := True;
   end Initialize;

   --  -----------------------
   --  Initialize the stream to write on the given stream.
   --  An internal buffer is allocated for writing the stream.
   --  -----------------------
--   overriding
   procedure Produces (Stream  : in out Encoder_Stream;
                       Output  : access Output_Stream'Class;
                       Size    : in Positive) is
   begin
      Stream.Initialize (Size);
      Stream.Output := Output;
      Stream.No_Flush := False;
   end Produces;

   --  -----------------------
   --  Initialize the stream to read the given streams.
   --  -----------------------
   procedure Consumes (Stream  : in out Encoder_Stream;
                       Input   : access Input_Stream'Class;
                       Size    : in Positive) is
   begin
      Stream.Initialize (Size);
      Stream.Input := Input;
   end Consumes;

   --  -----------------------
   --  Close the sink.
   --  -----------------------
   overriding
   procedure Close (Stream : in out Encoder_Stream) is
   begin
      Stream.Flush;
      if Stream.Output /= null then
         Stream.Output.Close;
      end if;
   end Close;

   --  -----------------------
   --  Write the buffer array to the output stream.
   --  -----------------------
   overriding
   procedure Write (Stream : in out Encoder_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
      First_Encoded : Ada.Streams.Stream_Element_Offset := Buffer'First;
      Last_Encoded  : Ada.Streams.Stream_Element_Offset;
      Last_Pos      : Ada.Streams.Stream_Element_Offset;
   begin
      while First_Encoded <= Buffer'Last loop
         Stream.Transform.Transform
           (Data    => Buffer (First_Encoded .. Buffer'Last),
            Into    => Stream.Buffer (Stream.Write_Pos .. Stream.Buffer'Last),
            Last    => Last_Pos,
            Encoded => Last_Encoded);
         if Last_Encoded < Buffer'Last or else Last_Pos = Stream.Buffer'Last then
            Stream.Output.Write (Stream.Buffer (Stream.Buffer'First .. Last_Pos));
            Stream.Write_Pos := Stream.Buffer'First;
         else
            Stream.Write_Pos := Last_Pos + 1;
         end if;
         First_Encoded := Last_Encoded + 1;
      end loop;
   end Write;

   --  ------------------------------
   --  Fill the buffer by reading the input stream.
   --  Raises Data_Error if there is no input stream;
   --  ------------------------------
   procedure Fill (Stream : in out Encoder_Stream) is
   begin
      if Stream.Input = null then
         Stream.Eof := True;
      else
         Stream.Input.Read (Stream.Buffer (1 .. Stream.Last), Stream.Write_Pos);
         Stream.Eof := Stream.Write_Pos < 1;
         if not Stream.Eof then
            Stream.Write_Pos := Stream.Write_Pos + 1;
         end if;
         Stream.Read_Pos := 1;
      end if;
   end Fill;

   --  -----------------------
   --  Read into the buffer as many bytes as possible and return in
   --  `last` the position of the last byte read.
   --  -----------------------
   overriding
   procedure Read (Stream : in out Encoder_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
      Start : Stream_Element_Offset := Into'First;
      Pos   : Stream_Element_Offset := Stream.Read_Pos;
      Avail : Stream_Element_Offset;
      Last_Decoded  : Ada.Streams.Stream_Element_Offset;
      Last_Pos      : Ada.Streams.Stream_Element_Offset;
   begin
      while Start <= Into'Last loop
         Avail := Stream.Write_Pos - Pos;
         if Avail = 0 then
            Stream.Fill;
            Pos := Stream.Read_Pos;
            Avail := Stream.Write_Pos - Pos;
            if Avail <= 0 then
               Last := Start - 1;
               Stream.Transform.Finish
                 (Into    => Into (Start .. Into'Last),
                  Last    => Last);
               return;
            end if;
         end if;
         Stream.Transform.Transform
           (Data    => Stream.Buffer (Pos .. Pos + Avail - 1),
            Into    => Into (Start .. Into'Last),
            Last    => Last_Pos,
            Encoded => Last_Decoded);
         Stream.Read_Pos := Last_Decoded + 1;
         Start := Last_Pos + 1;
         exit when Stream.Read_Pos = Pos;
         Pos := Stream.Read_Pos;
      end loop;
      Last := Start - 1;
   end Read;

   --  -----------------------
   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   --  -----------------------
   overriding
   procedure Flush (Stream : in out Encoder_Stream) is
      Last_Pos : Ada.Streams.Stream_Element_Offset := Stream.Write_Pos - 1;
   begin
      if not Stream.Flushed
        and then Stream.Buffer /= null
        and then Stream.Write_Pos >= Stream.Buffer'First
      then
         Stream.Transform.Finish (Stream.Buffer (Stream.Write_Pos .. Stream.Buffer'Last),
                                  Last_Pos);
         Stream.Write_Pos := Last_Pos + 1;
         if not Stream.No_Flush then
            if Stream.Write_Pos > 1 then
               if Stream.Output /= null then
                  Stream.Output.Write (Stream.Buffer (1 .. Stream.Write_Pos - 1));
               end if;
               Stream.Write_Pos := 1;
            end if;
            if Stream.Output /= null then
               Stream.Output.Flush;
            end if;
         end if;
         Stream.Flushed := True;
      end if;
   end Flush;

   overriding
   procedure Finalize (Stream : in out Encoder_Stream) is
   begin
      if not Stream.Flushed and then Stream.Buffer /= null then
         Stream.Flush;
      end if;
      Buffer_Stream (Stream).Finalize;
   end Finalize;

end Util.Streams.Buffered.Encoders;
