-----------------------------------------------------------------------
--  util-streams-encoders -- Streams with encoding and decoding capabilities
--  Copyright (C) 2017 Stephane Carrez
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

with Util.Encoders.Base64;
package body Util.Streams.Buffered.Encoders is

   --  -----------------------
   --  Initialize the stream to write on the given stream.
   --  An internal buffer is allocated for writing the stream.
   --  -----------------------
   procedure Initialize (Stream  : in out Encoding_Stream;
                         Output  : in Output_Stream_Access;
                         Size    : in Natural;
                         Format  : in String) is
   begin
      Stream.Initialize (Output, Size);
      Stream.Transform := new Util.Encoders.Base64.Encoder;
   end Initialize;

   --  -----------------------
   --  Close the sink.
   --  -----------------------
   overriding
   procedure Close (Stream : in out Encoding_Stream) is
   begin
      Stream.Flush;
      Stream.Output.Close;
   end Close;

   --  -----------------------
   --  Write the buffer array to the output stream.
   --  -----------------------
   overriding
   procedure Write (Stream : in out Encoding_Stream;
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
         if Last_Encoded < Buffer'Last then
            Stream.Output.Write (Stream.Buffer (Stream.Buffer'First .. Last_Pos));
            Stream.Write_Pos := Stream.Buffer'First;
         else
            Stream.Write_Pos := Last_Pos + 1;
         end if;
         First_Encoded := Last_Encoded + 1;
      end loop;
   end Write;

   --  -----------------------
   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   --  -----------------------
   overriding
   procedure Flush (Stream : in out Encoding_Stream) is
      Last_Pos : Ada.Streams.Stream_Element_Offset;
   begin
      Stream.Transform.Finish (Stream.Buffer (Stream.Write_Pos .. Stream.Buffer'Last),
                               Last_Pos);
      Stream.Write_Pos := Last_Pos + 1;
      Output_Buffer_Stream (Stream).Flush;
   end Flush;

   --  -----------------------
   --  Flush the stream and release the buffer.
   --  -----------------------
   overriding
   procedure Finalize (Object : in out Encoding_Stream) is
   begin
      null;
   end Finalize;

end Util.Streams.Buffered.Encoders;
