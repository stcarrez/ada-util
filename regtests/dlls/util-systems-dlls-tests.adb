-----------------------------------------------------------------------
--  util-systems-dlls-tests -- Unit tests for shared libraries
--  Copyright (C) 2013, 2017, 2019, 2022 Stephane Carrez
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

with Util.Test_Caller;
package body Util.Systems.DLLs.Tests is

   use Util.Tests;
   use type System.Address;

   procedure Load_Library (T : in out Test;
                           Lib : out Handle);

   package Caller is new Util.Test_Caller (Test, "Systems.Dlls");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Systems.Dlls.Load",
                       Test_Load'Access);
      Caller.Add_Test (Suite, "Test Util.Systems.Dlls.Get_Symbol",
                       Test_Get_Symbol'Access);
   end Add_Tests;

   procedure Load_Library (T : in out Test;
                           Lib : out Handle) is
      Lib1 : Handle;
      Lib2 : Handle;
      Lib3 : Handle;
      Lib4 : Handle;
      Lib5 : Handle;
      Lib6 : Handle;
   begin
      begin
         Lib1 := Util.Systems.DLLs.Load ("libcrypto.so");
         T.Assert (Lib1 /= Null_Handle, "Load operation returned null");
         Lib := Lib1;
      exception
         when Load_Error =>
            Lib1 := Null_Handle;
      end;

      begin
         Lib2 := Util.Systems.DLLs.Load ("libgmp.dylib");
         T.Assert (Lib2 /= Null_Handle, "Load operation returned null");
         Lib := Lib2;
      exception
         when Load_Error =>
            Lib2 := Null_Handle;
      end;

      begin
         Lib3 := Util.Systems.DLLs.Load ("zlib1.dll");
         T.Assert (Lib3 /= Null_Handle, "Load operation returned null");
         Lib := Lib3;
      exception
         when Load_Error =>
            Lib3 := Null_Handle;
      end;

      begin
         Lib4 := Util.Systems.DLLs.Load ("libz.so");
         T.Assert (Lib4 /= Null_Handle, "Load operation returned null");
         Lib := Lib4;
      exception
         when Load_Error =>
            Lib4 := Null_Handle;
      end;

      begin
         Lib5 := Util.Systems.DLLs.Load ("libgmp.so");
         T.Assert (Lib5 /= Null_Handle, "Load operation returned null");
         Lib := Lib5;
      exception
         when Load_Error =>
            Lib5 := Null_Handle;
      end;

      begin
         Lib6 := Util.Systems.DLLs.Load ("libexpat-1.dll");
         T.Assert (Lib6 /= Null_Handle, "Load operation returned null");
         Lib := Lib6;
      exception
         when Load_Error =>
            Lib6 := Null_Handle;
      end;

      T.Assert (Lib1 /= Null_Handle
                  or else Lib2 /= Null_Handle
                  or else Lib3 /= Null_Handle
                  or else Lib4 /= Null_Handle
                  or else Lib5 /= Null_Handle
                  or else Lib6 /= Null_Handle,
                "At least one Load operation should have succeeded");
   end Load_Library;

   --  ------------------------------
   --  Test the loading a shared library.
   --  ------------------------------
   procedure Test_Load (T : in out Test) is
      Lib : Handle;
   begin
      Load_Library (T, Lib);
      begin
         Lib := Util.Systems.DLLs.Load ("some-invalid-library");

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
      Sym : System.Address := System.Null_Address;
   begin
      Load_Library (T, Lib);
      T.Assert (Lib /= Null_Handle, "Load operation returned null");

      begin
         Sym := Util.Systems.DLLs.Get_Symbol (Lib, "EVP_sha1");
         T.Assert (Sym /= System.Null_Address, "Get_Symbol returned null");

      exception
         when Not_Found =>
            null;
      end;

      begin
         Sym := Util.Systems.DLLs.Get_Symbol (Lib, "compress");
         T.Assert (Sym /= System.Null_Address, "Get_Symbol returned null");

      exception
         when Not_Found =>
            null;
      end;

      begin
         Sym := Util.Systems.DLLs.Get_Symbol (Lib, "__gmpf_cmp");
         T.Assert (Sym /= System.Null_Address, "Get_Symbol returned null");

      exception
         when Not_Found =>
            null;
      end;

      begin
         Sym := Util.Systems.DLLs.Get_Symbol (Lib, "XML_ParserCreate");
         T.Assert (Sym /= System.Null_Address, "Get_Symbol returned null");

      exception
         when Not_Found =>
            null;
      end;

      --  We must have found one of the two symbols
      T.Assert (Sym /= System.Null_Address, "Get_Symbol returned null");

      begin
         Sym := Util.Systems.DLLs.Get_Symbol (Lib, "some-invalid-symbol");
         T.Fail ("The Get_Symbol operation must raise an exception");

      exception
         when Not_Found =>
            null;

      end;
   end Test_Get_Symbol;

end Util.Systems.DLLs.Tests;
