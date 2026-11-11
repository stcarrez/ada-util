-----------------------------------------------------------------------
--  util-encoders-random-system -- System random generator (/dev/urandom)
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Calendar;
with Interfaces.C.Strings;
with Util.Streams.Raw;
with Util.Systems.Os;
with Util.Dates;

package body Util.Encoders.Random.System is

   use type Util.Systems.Os.File_Type;

   --  ------------------------------
   --  Fill the array with random bytes using the system generator.
   --  ------------------------------
   overriding
   procedure Generate (Gen  : in out System_Generator;
                       Into : out Ada.Streams.Stream_Element_Array) is

   begin
      if Gen.Use_Dev_Random then
         declare
            Path   : Interfaces.C.Strings.chars_ptr
              := Interfaces.C.Strings.New_String ("/dev/urandom");
            Fd     : constant Util.Systems.Os.File_Type
              := Util.Systems.Os.Sys_Open (Path, Util.Systems.Os.O_RDONLY, 0);
         begin
            Interfaces.C.Strings.Free (Path);
            if Fd /= Util.Systems.Os.NO_FILE then
               declare
                  Stream : Util.Streams.Raw.Raw_Stream;
                  Last   : Ada.Streams.Stream_Element_Offset;
               begin
                  --  The raw stream owns the descriptor and closes it on finalize.
                  Stream.Initialize (Fd);
                  Stream.Read (Into, Last);
                  while Last < Into'Last loop
                     Stream.Read (Into (Last + 1 .. Into'Last), Last);
                  end loop;
                  return;
               exception
                  when others =>
                     null;
               end;
            end if;
         end;
         Gen.Use_Dev_Random := False;
         Gen.Rand.Set_Seed
           (Interfaces.Unsigned_64 (Util.Dates.To_Nanoseconds (Ada.Calendar.Clock)));
      end if;
      Gen.Rand.Generate (Into);
   end Generate;

end Util.Encoders.Random.System;
