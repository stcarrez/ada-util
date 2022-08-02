-----------------------------------------------------------------------
--  compress -- Compress file using Util.Streams.Buffered.LZMA
--  Copyright (C) 2019, 2021, 2022 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Util.Streams.Files;
with Util.Streams.Lzma;
procedure Compress is

   procedure Compress_File (Source      : in String;
                            Destination : in String);

   procedure Compress_File (Source      : in String;
                            Destination : in String) is
      In_Stream  : aliased Util.Streams.Files.File_Stream;
      Out_Stream : aliased Util.Streams.Files.File_Stream;
      Compressor : aliased Util.Streams.Lzma.Compress_Stream;
   begin
      In_Stream.Open (Mode => Ada.Streams.Stream_IO.In_File, Name => Source);
      Out_Stream.Create (Mode => Ada.Streams.Stream_IO.Out_File, Name => Destination);
      Compressor.Initialize (Output => Out_Stream'Unchecked_Access, Size => 32768);
      Util.Streams.Copy (From => In_Stream, Into => Compressor);
   end Compress_File;

begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: compress source destination");
      return;
   end if;

   Compress_File (Source      => Ada.Command_Line.Argument (1),
                  Destination => Ada.Command_Line.Argument (2));
end Compress;
