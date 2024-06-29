-----------------------------------------------------------------------
--  util-streams-files -- File Stream utilities
--  Copyright (C) 2010, 2013, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Streams.Files is

   --  ------------------------------
   --  Open the file and initialize the stream for reading or writing.
   --  ------------------------------
   procedure Open (Stream  : in out File_Stream;
                   Mode    : in Ada.Streams.Stream_IO.File_Mode;
                   Name    : in String := "";
                   Form    : in String := "") is
   begin
      Ada.Streams.Stream_IO.Open (Stream.File, Mode, Name, Form);
   end Open;

   --  ------------------------------
   --  Create the file and initialize the stream for writing.
   --  ------------------------------
   procedure Create (Stream  : in out File_Stream;
                     Mode    : in Ada.Streams.Stream_IO.File_Mode;
                     Name    : in String := "";
                     Form    : in String := "") is
   begin
      Ada.Streams.Stream_IO.Create (Stream.File, Mode, Name, Form);
   end Create;

   --  ------------------------------
   --  Close the stream.
   --  ------------------------------
   overriding
   procedure Close (Stream : in out File_Stream) is
   begin
      Ada.Streams.Stream_IO.Close (Stream.File);
   end Close;

   --  ------------------------------
   --  Write the buffer array to the output stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out File_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      Ada.Streams.Stream_IO.Write (Stream.File, Buffer);
   end Write;

   --  ------------------------------
   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   --  ------------------------------
   overriding
   procedure Read (Stream : in out File_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      Ada.Streams.Stream_IO.Read (Stream.File, Into, Last);
   end Read;

   --  ------------------------------
   --  Flush the stream and release the buffer.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out File_Stream) is
   begin
      if Ada.Streams.Stream_IO.Is_Open (Object.File) then
         Object.Close;
      end if;
   end Finalize;

end Util.Streams.Files;
