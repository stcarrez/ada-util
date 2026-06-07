-----------------------------------------------------------------------
--  util-files-rolling-lzma -- Rolling file manager with LZMA compression
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Util.Streams.Files;
with Util.Streams.Lzma;
package body Util.Files.Rolling.Lzma is

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

   overriding
   procedure Rename (Manager  : in out Lzma_File_Manager;
                     Old_Name : in String;
                     New_Name : in String) is
      pragma Unreferenced (Manager);
   begin
      Compress_File (Old_Name, New_Name);
      Ada.Directories.Delete_File (Old_Name);
   end Rename;

end Util.Files.Rolling.Lzma;
