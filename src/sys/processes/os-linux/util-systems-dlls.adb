-----------------------------------------------------------------------
--  util-systems-dlls -- Unix shared library support
--  Copyright (C) 2013 Stephane Carrez
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

with Interfaces.C.Strings;

package body Util.Systems.DLLs is

   pragma Linker_Options (Util.Systems.Constants.DLL_OPTIONS);

   function Sys_Dlopen (Path  : in Interfaces.C.Strings.chars_ptr;
                        Mode  : in Flags) return Handle;
   pragma Import (C, Sys_Dlopen, "dlopen");

   function Sys_Dlclose (Lib : in Handle) return Interfaces.C.int;
   pragma Import (C, Sys_Dlclose, "dlclose");

   function Sys_Dlsym (Lib    : in Handle;
                       Symbol : in Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, Sys_Dlsym, "dlsym");

   function Sys_Dlerror return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Sys_Dlerror, "dlerror");

   function Error_Message return String;

   function Error_Message return String is
   begin
      return Interfaces.C.Strings.Value (Sys_Dlerror);
   end Error_Message;

   --  -----------------------
   --  Load the shared library with the given name or path and return a library handle.
   --  Raises the <tt>Load_Error</tt> exception if the library cannot be loaded.
   --  -----------------------
   function Load (Path : in String;
                  Mode : in Flags := Util.Systems.Constants.RTLD_LAZY) return Handle is
      Lib    : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Path);
      Result : constant Handle := Sys_Dlopen (Lib, Mode);
   begin
      Interfaces.C.Strings.Free (Lib);
      if Result = Null_Handle then
         raise Load_Error with Error_Message;
      else
         return Result;
      end if;
   end Load;

   --  -----------------------
   --  Unload the shared library.
   --  -----------------------
   procedure Unload (Lib : in Handle) is
      Result : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin
      if Lib /= Null_Handle then
         Result := Sys_Dlclose (Lib);
      end if;
   end Unload;

   --  -----------------------
   --  Get a global symbol with the given name in the library.
   --  Raises the <tt>Not_Found</tt> exception if the symbol does not exist.
   --  -----------------------
   function Get_Symbol (Lib  : in Handle;
                        Name : in String) return System.Address is
      use type System.Address;

      Symbol : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Name);
      Result : constant System.Address := Sys_Dlsym (Lib, Symbol);
   begin
      Interfaces.C.Strings.Free (Symbol);
      if Result = System.Null_Address then
         raise Not_Found with Error_Message;
      else
         return Result;
      end if;
   end Get_Symbol;

end Util.Systems.DLLs;
