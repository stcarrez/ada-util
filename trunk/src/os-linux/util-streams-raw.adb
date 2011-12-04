-----------------------------------------------------------------------
--  util-streams-raw -- Raw streams (OS specific)
--  Copyright (C) 2011 Stephane Carrez
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

with Ada.IO_Exceptions;

package body Util.Streams.Raw is

   use Util.Systems.Os;

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
   --  Close the stream.
   --  -----------------------
   overriding
   procedure Close (Stream : in out Raw_Stream) is
   begin
      if Stream.File /= NO_FILE then
         if Close (Stream.File) /= 0 then
            raise Ada.IO_Exceptions.Device_Error;
         end if;
         Stream.File := NO_FILE;
      end if;
   end Close;

   --  -----------------------
   --  Write the buffer array to the output stream.
   --  -----------------------
   procedure Write (Stream : in out Raw_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      if Write (Stream.File, Buffer'Address, Buffer'Length) < 0 then
         raise Ada.IO_Exceptions.Device_Error;
      end if;
   end Write;

   --  -----------------------
   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   --  -----------------------
   procedure Read (Stream : in out Raw_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
      Res : Ssize_T;
   begin
      Res := Read (Stream.File, Into'Address, Into'Length);
      if Res < 0 then
         raise Ada.IO_Exceptions.Device_Error;
      end if;
      Last := Into'First + Ada.Streams.Stream_Element_Offset (Res) - 1;
   end Read;

   --  -----------------------
   --  Flush the stream and release the buffer.
   --  -----------------------
   procedure Finalize (Object : in out Raw_Stream) is
   begin
      Close (Object);
   end Finalize;

end Util.Streams.Raw;
