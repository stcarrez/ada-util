-----------------------------------------------------------------------
--  util-encoders-lzma -- LZMA compression and decompression
--  Copyright (C) 2018, 2021, 2022 Stephane Carrez
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
with Lzma.Check;
with Lzma.Container;
with Interfaces.C;

package body Util.Encoders.Lzma is

   use type Interfaces.C.size_t;
   --  use type Ada.Streams.Stream_Element_Offset;
   use type Base.lzma_ret;

   subtype Offset is Ada.Streams.Stream_Element_Offset;

   --  ------------------------------
   --  Compress the binary input stream represented by <b>Data</b> by using
   --  the LZMA compression into the output stream <b>Into</b>.
   --
   --  If the transformer does not have enough room to write the result,
   --  it must return in <b>Encoded</b> the index of the last encoded
   --  position in the <b>Data</b> stream.
   --
   --  The transformer returns in <b>Last</b> the last valid position
   --  in the output stream <b>Into</b>.
   --
   --  The <b>Encoding_Error</b> exception is raised if the input
   --  stream cannot be transformed.
   --  ------------------------------
   overriding
   procedure Transform (E       : in out Compress;
                        Data    : in Ada.Streams.Stream_Element_Array;
                        Into    : out Ada.Streams.Stream_Element_Array;
                        Last    : out Ada.Streams.Stream_Element_Offset;
                        Encoded : out Ada.Streams.Stream_Element_Offset) is
      Result : Base.lzma_ret;
   begin
      E.Stream.next_out := Into (Into'First)'Unchecked_Access;
      E.Stream.avail_out := Into'Length;
      E.Stream.next_in := Data (Data'First)'Unrestricted_Access;
      E.Stream.avail_in := Interfaces.C.size_t (Data'Length);
      loop
         Result := Base.lzma_code (E.Stream'Unchecked_Access, Base.LZMA_RUN);

         --  Write the output data when the buffer is full or we reached the end of stream.
         if E.Stream.avail_out = 0
           or else E.Stream.avail_in = 0
           or else Result = Base.LZMA_STREAM_END
         then
            Last := Into'First + Into'Length - Offset (E.Stream.avail_out) - 1;
            Encoded := Data'First + Data'Length - Offset (E.Stream.avail_in);
            return;
         end if;
         exit when Result /= Base.LZMA_OK;
      end loop;
      Encoded := Into'First;
   end Transform;

   --  ------------------------------
   --  Finish compression of the input array.
   --  ------------------------------
   overriding
   procedure Finish (E    : in out Compress;
                     Into : in out Ada.Streams.Stream_Element_Array;
                     Last : in out Ada.Streams.Stream_Element_Offset) is
      Result : Base.lzma_ret;
      pragma Unreferenced (Result);
   begin
      E.Stream.next_out := Into (Into'First)'Unchecked_Access;
      E.Stream.avail_out := Into'Length;
      E.Stream.next_in := null;
      E.Stream.avail_in := 0;
      Result := Base.lzma_code (E.Stream'Unchecked_Access, Base.LZMA_FINISH);
      Last := Into'First + Into'Length - Offset (E.Stream.avail_out) - 1;
   end Finish;

   overriding
   procedure Initialize (E : in out Compress) is
      Result : Base.lzma_ret;
      pragma Unreferenced (Result);
   begin
      Result := Container.lzma_easy_encoder (E.Stream'Unchecked_Access, 6,
                                             Check.LZMA_CHECK_CRC64);
   end Initialize;

   overriding
   procedure Finalize (E : in out Compress) is
   begin
      Base.lzma_end (E.Stream'Unchecked_Access);
   end Finalize;

end Util.Encoders.Lzma;
