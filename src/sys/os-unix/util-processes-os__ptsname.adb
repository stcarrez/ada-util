-----------------------------------------------------------------------
--  util-processes-os__ptsname.adb -- Ptsname using ptsname(3)
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

separate (Util.Processes.Os) function Ptsname (Fd     : in File_Type;
                                               Buf    : out Ptr) return Integer is
   use Interfaces.C.Strings;

   Name : constant Ptr := Sys_Ptsname (Fd);
begin
   if Name = Null_Ptr then
      Buf := Null_Ptr;
      return -1;
   end if;
   Buf := New_String (Value (Name));
   return 0;
end Ptsname;
