-----------------------------------------------------------------------
--  util-streams-raw -- Raw streams for Windows based systems
--  Copyright (C) 2011, 2019, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.IO_Exceptions;

with System;

package body Util.Streams.Raw is

   use System;
   use Util.Systems.Os;
   use type Util.Systems.Os.HANDLE;

   --  -----------------------
   --  Initialize the raw stream to read and write on the given file descriptor.
   --  -----------------------
   procedure Initialize (Stream  : in out Raw_Stream;
                         File    : in File_Type) is
   begin
      if Stream.File /= NO_FILE then
         raise Ada.IO_Exceptions.Status_Error;
      end if;
      Stream.File := File;
   end Initialize;

   --  -----------------------
   --  Get the file descriptor associated with the stream.
   --  -----------------------
   function Get_File (Stream : in Raw_Stream) return Util.Systems.Os.File_Type is
   begin
      return Stream.File;
   end Get_File;

   --  -----------------------
   --  Set the file descriptor to be used by the stream.
   --  -----------------------
   procedure Set_File (Stream : in out Raw_Stream;
                       File   : in Util.Systems.Os.File_Type) is
   begin
      Stream.File := File;
   end Set_File;

   --  -----------------------
   --  Close the stream.
   --  -----------------------
   overriding
   procedure Close (Stream : in out Raw_Stream) is
      Error : Integer;
   begin
      if Stream.File /= NO_FILE then
         if Close_Handle (Stream.File) = 0 then
            Error := Get_Last_Error;
            if Error /= ERROR_BROKEN_PIPE then
               raise Ada.IO_Exceptions.Device_Error with "IO error: " & Integer'Image (Error);
            end if;
         end if;
         Stream.File := NO_FILE;
      end if;
   end Close;

   --  -----------------------
   --  Write the buffer array to the output stream.
   --  -----------------------
   overriding
   procedure Write (Stream : in out Raw_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
      Res    : aliased DWORD := 0;
      Status : BOOL;
   begin
      Status := Write_File (Stream.File, Buffer'Address, Buffer'Length,
                            Res'Unchecked_Access, System.Null_Address);
      if Status = 0 then
         raise Ada.IO_Exceptions.Device_Error;
      end if;
   end Write;

   --  -----------------------
   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   --  -----------------------
   overriding
   procedure Read (Stream : in out Raw_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
      Res    : aliased DWORD := 0;
      Status : BOOL;
      Error  : Integer;
   begin
      Status := Read_File (Stream.File, Into'Address, Into'Length,
                           Res'Unchecked_Access, System.Null_Address);
      if Status = 0 then
         Error := Get_Last_Error;
         if Error /= ERROR_BROKEN_PIPE then
            raise Ada.IO_Exceptions.Device_Error with "IO error: " & Integer'Image (Error);
         end if;
      end if;
      Last := Into'First + Ada.Streams.Stream_Element_Offset (Res) - 1;
   end Read;

   --  -----------------------
   --  Reposition the read/write file offset.
   --  -----------------------
   procedure Seek (Stream : in out Raw_Stream;
                   Pos    : in Util.Systems.Types.off_t;
                   Mode   : in Util.Systems.Types.Seek_Mode) is
      Res : Util.Systems.Types.off_t;
   begin
      Res := Sys_Lseek (Stream.File, Pos, Mode);
      if Res < 0 then
         raise Ada.IO_Exceptions.Device_Error;
      end if;
   end Seek;

   --  -----------------------
   --  Flush the stream and release the buffer.
   --  -----------------------
   overriding
   procedure Finalize (Object : in out Raw_Stream) is
   begin
      Close (Object);
   end Finalize;

end Util.Streams.Raw;
