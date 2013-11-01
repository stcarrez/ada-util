-----------------------------------------------------------------------
--  util-systems-dlls-tests -- Unit tests for shared libraries
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

with Ada.Unchecked_Deallocation;
with Util.Test_Caller;
with Util.Systems.Os;
package body Util.Systems.Dlls.Tests is

   use Util.Tests;
   use type System.Address;

   package Caller is new Util.Test_Caller (Test, "Systems.Dlls");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Systems.Dlls.Load",
                       Test_Load'Access);
      Caller.Add_Test (Suite, "Test Util.Systems.Dlls.Get_Symbol",
                       Test_Get_Symbol'Access);
   end Add_Tests;

   function Get_Test_Library return String is
   begin
      if Util.Systems.Os.Directory_Separator = '/' then
         return "libcrypto.so";
      else
         return "";
      end if;
   end Get_Test_Library;

   function Get_Test_Symbol return String is
   begin
      if Util.Systems.Os.Directory_Separator = '/' then
         return "inflate";
      else
         return "";
      end if;
   end Get_Test_Symbol;

   --  ------------------------------
   --  Test the loading a shared library.
   --  ------------------------------
   procedure Test_Load (T : in out Test) is
      Lib : Handle;
   begin
      Lib := Util.Systems.Dlls.Load (Get_Test_Library);
      T.Assert (Lib /= Null_Handle, "Load operation returned null");

      begin
         Lib := Util.Systems.Dlls.Load ("some-invalid-library");

         T.Fail ("Load must raise an exception");

      exception
         when Load_Error =>
            null;
      end;
   end Test_Load;

   --  ------------------------------
   --  Test getting a shared library symbol.
   --  ------------------------------
   procedure Test_Get_Symbol (T : in out Test) is
      Lib : Handle;
      Sym : System.Address;
   begin
      Lib := Util.Systems.Dlls.Load (Get_Test_Library);
      T.Assert (Lib /= Null_Handle, "Load operation returned null");

      Sym := Util.Systems.Dlls.Get_Symbol (Lib, Get_Test_Symbol);
      T.Assert (Sym /= System.Null_Address, "Get_Symbol returned null");

      begin
         Sym := Util.Systems.Dlls.Get_Symbol (Lib, "some-invalid-symbol");
         T.Fail ("The Get_Symbol operation must raise an exception");

      exception
         when Not_Found =>
            null;

      end;
   end Test_Get_Symbol;

end Util.Systems.Dlls.Tests;
