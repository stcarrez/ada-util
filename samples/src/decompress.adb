-----------------------------------------------------------------------
--  decompress -- Decompress file using Util.Streams.Buffered.LZMA
--  Copyright (C) 2019, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Util.Streams.Files;
with Util.Streams.Lzma;
procedure Decompress is

   procedure Decompress_File (Source      : in String;
                              Destination : in String);

   procedure Decompress_File (Source      : in String;
                              Destination : in String) is
      In_Stream    : aliased Util.Streams.Files.File_Stream;
      Out_Stream   : aliased Util.Streams.Files.File_Stream;
      Decompressor : aliased Util.Streams.Lzma.Decompress_Stream;
   begin
      In_Stream.Open (Mode => Ada.Streams.Stream_IO.In_File, Name => Source);
      Out_Stream.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Destination);
      Decompressor.Initialize (Input => In_Stream'Unchecked_Access, Size => 32768);
      Util.Streams.Copy (From => Decompressor, Into => Out_Stream);
   end Decompress_File;

begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: decompress source destination");
      return;
   end if;

   Decompress_File (Source => Ada.Command_Line.Argument (1),
                    Destination => Ada.Command_Line.Argument (2));
end Decompress;
