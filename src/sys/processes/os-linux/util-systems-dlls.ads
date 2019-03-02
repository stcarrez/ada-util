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

with System;

with Interfaces.C;
with Util.Systems.Constants;

package Util.Systems.DLLs is

   --  The shared library handle.
   type Handle is private;

   Null_Handle : constant Handle;

   --  Exception raised when there is a problem loading a shared library.
   Load_Error : exception;

   --  Exception raised when a symbol cannot be found in a shared library.
   Not_Found  : exception;

   subtype Flags is Interfaces.C.int;

   Extension  : constant String := ".so";

   --  Load the shared library with the given name or path and return a library handle.
   --  Raises the <tt>Load_Error</tt> exception if the library cannot be loaded.
   function Load (Path : in String;
                  Mode : in Flags := Util.Systems.Constants.RTLD_LAZY) return Handle;

   --  Unload the shared library.
   procedure Unload (Lib : in Handle);

   --  Get a global symbol with the given name in the library.
   --  Raises the <tt>Not_Found</tt> exception if the symbol does not exist.
   function Get_Symbol (Lib  : in Handle;
                        Name : in String) return System.Address;

private

   type Handle is new System.Address;

   Null_Handle : constant Handle := Handle (System.Null_Address);

end Util.Systems.DLLs;
