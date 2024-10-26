-----------------------------------------------------------------------
--  util-processes-os__ptsname.adb -- Ptsname using ptsname(3)
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

separate (Util.Processes.Os) function Ptsname (Fd     : in File_Type;
                                               Buf    : in Ptr;
                                               Buflen : in Size_T) return Integer is
   use Interfaces.C.Strings;

   Name : constant Ptr := Sys_Ptsname (Fd);
   Len  : Size_T;
begin
   if Name = Null_Ptr then
      return -1;
   end if;
   Len := Size_T (Strlen (Name));
   if Len > Buflen then
      return -1;
   end if;
   Interfaces.C.Strings.Update (Buf, 0, String '(Value (Name)), False);
   return 0;
end Ptsname;
