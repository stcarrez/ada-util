-----------------------------------------------------------------------
--  util-processes-tests - Test for processes
--  Copyright (C) 2011, 2012, 2016, 2018, 2019, 2021, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Directories;
with Util.Test_Caller;
with Util.Strings.Vectors;
with Util.Processes.Tools;
package body Util.Samples_Tests is

   use Util.Tests;
   use type Util.Strings.Vectors.Vector;

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   package Caller is new Util.Test_Caller (Test, "Samples");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test encodes",
                       Test_Encodes'Access);
      Caller.Add_Test (Suite, "Test cut",
                       Test_Cut'Access);
      Caller.Add_Test (Suite, "Test escape",
                       Test_Escape'Access);
      Caller.Add_Test (Suite, "Test csv_city",
                       Test_Csv_City'Access);
      Caller.Add_Test (Suite, "Test objcalc",
                       Test_Objcalc'Access);
      Caller.Add_Test (Suite, "Test log",
                       Test_Log'Access);
      Caller.Add_Test (Suite, "Test copy",
                       Test_Copy'Access);
      if Ada.Directories.Exists ("bin/compress") then
         Caller.Add_Test (Suite, "Test compress/decompress",
                          Test_Compress_Decompress'Access);
         Caller.Add_Test (Suite, "Test lzma_encrypt/lzma_decrypt",
                          Test_Lzma_Encrypt_Decrypt'Access);
         Caller.Add_Test (Suite, "Test lzma_encrypt_b64/lzma_decrypt_b64",
                          Test_Lzma_Encrypt_Decrypt_B64'Access);
      end if;
      Caller.Add_Test (Suite, "Test encrypt/decrypt",
                       Test_Encrypt_Decrypt'Access);
      Caller.Add_Test (Suite, "Test multipart",
                       Test_Multipart'Access);
      Caller.Add_Test (Suite, "Test dumpcert",
                       Test_Dumpcert'Access);
      Caller.Add_Test (Suite, "Test sha256",
                       Test_Sha256'Access);
      Caller.Add_Test (Suite, "Test env",
                       Test_Env'Access);
      Caller.Add_Test (Suite, "Test serialize",
                       Test_Serialize'Access);
      Caller.Add_Test (Suite, "Test serialize_xml",
                       Test_Serialize_XML'Access);
      Caller.Add_Test (Suite, "Test proplist",
                       Test_Proplist'Access);
      Caller.Add_Test (Suite, "Test properties",
                       Test_Properties'Access);
   end Add_Tests;

   --  ------------------------------
   --  Tests the encodes example.
   --  ------------------------------
   procedure Test_Encodes (T : in out Test) is
      Result : UString;
   begin
      T.Execute ("bin/encodes base64 hello", Result);
      Assert_Matches (T, "Encodes base64: aGVsbG8=", Result);

      T.Execute ("bin/encodes base64 -d aGVsbG8=", Result);
      Assert_Matches (T, "Decodes base64: hello", Result);

      T.Execute ("bin/encodes base16 hello", Result);
      Assert_Matches (T, "Encodes base16: 68656C6C6F", Result);

      T.Execute ("bin/encodes base16 -d 68656C6C6F", Result);
      Assert_Matches (T, "Decodes base16: hello", Result);

      T.Execute ("bin/encodes base32 hello", Result);
      Assert_Matches (T, "Encodes base32: NBSWY3DP", Result);

      T.Execute ("bin/encodes base32 -d NBSWY3DP", Result);
      Assert_Matches (T, "Decodes base32: hello", Result);

      T.Execute ("bin/encodes sha1 hello", Result);
      Assert_Matches (T, "Encodes sha1: AAF4C61DDCC5E8A2DABEDE0F3B482CD9AEA9434D", Result);
   end Test_Encodes;

   --  ------------------------------
   --  Tests the cut example.
   --  ------------------------------
   procedure Test_Cut (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
      Expect : Util.Strings.Vectors.Vector := "a" & "b";
   begin
      Expect := Expect & "c" & "d";
      Util.Processes.Tools.Execute ("bin/cut : a:b:c:d", List, Status);
      Assert_Equals (T, 0, Status, "Invalid execution status");
      Assert_Equal_Vectors (T, Expect, List, "invalid cut output");
   end Test_Cut;

   --  ------------------------------
   --  Tests the escape example.
   --  ------------------------------
   procedure Test_Escape (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
      Expect : constant Util.Strings.Vectors.Vector :=
         "Escape javascript : \'<xml>\'"
       & "Escape XML        : &apos;&lt;xml&gt;&apos;";
   begin
      Util.Processes.Tools.Execute ("bin/escape ""'<xml>'""", List, Status);
      Assert_Equals (T, 0, Status, "Invalid execution status");
      Assert_Equal_Vectors (T, Expect, List, "invalid escape output");
   end Test_Escape;

   --  ------------------------------
   --  Tests the csv_city example.
   --  ------------------------------
   procedure Test_Csv_City (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
      Expect : Util.Strings.Vectors.Vector;
   begin
      Expect := Expect
        & "Found  101 cities"
        & "City        : Caseras"
        & "Country code: es"
        & "Region      : 34"
        & "Latitude    :  4.35333E+01"
        & "Longitude   : -6.05000E+00";
      Util.Processes.Tools.Execute ("bin/csv_city samples/cities.csv caseras", List, Status);
      Assert_Equals (T, 0, Status, "Invalid execution status");
      Assert_Equal_Vectors (T, Expect, List, "invalid csv_city output");
   end Test_Csv_City;

   --  ------------------------------
   --  Tests the objcalc example.
   --  ------------------------------
   procedure Test_Objcalc (T : in out Test) is
      Result : UString;
   begin
      T.Execute ("bin/objcalc", Result);
      Assert_Matches (T, "111", Result);
   end Test_Objcalc;

   --  ------------------------------
   --  Tests the log example.
   --  ------------------------------
   procedure Test_Log (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
      Expect : constant Util.Strings.Vectors.Vector :=
         "WARN : A warning message"
       & "ERROR: An error message";
   begin
      Util.Processes.Tools.Execute ("bin/log", List, Status);
      Assert_Equals (T, 0, Status, "Invalid execution status");
      Assert_Equal_Vectors (T, Expect, List, "invalid log output");
   end Test_Log;

   --  ------------------------------
   --  Tests the copy example.
   --  ------------------------------
   procedure Test_Copy (T : in out Test) is
      Path   : constant String := Util.Tests.Get_Test_Path ("copy.txt");
      Result : UString;
   begin
      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
      T.Execute ("bin/copy Makefile " & Path, Result);
      Assert_Equal_Files (T, "Makefile", Path, "Copy failed");
   end Test_Copy;

   --  ------------------------------
   --  Tests the compress/decompress example.
   --  ------------------------------
   procedure Test_Compress_Decompress (T : in out Test) is
      Path1  : constant String := Util.Tests.Get_Test_Path ("copy.txt.xz");
      Path2  : constant String := Util.Tests.Get_Test_Path ("copy.txt");
      Result : UString;
   begin
      if Ada.Directories.Exists (Path1) then
         Ada.Directories.Delete_File (Path1);
      end if;
      if Ada.Directories.Exists (Path2) then
         Ada.Directories.Delete_File (Path2);
      end if;
      T.Execute ("bin/compress Makefile " & Path1, Result);
      T.Execute ("bin/decompress " & Path1 & " " & Path2, Result);
      Assert_Equal_Files (T, "Makefile", Path2, "Compress+Decompress failed");
   end Test_Compress_Decompress;

   --  ------------------------------
   --  Tests the encrypt/decrypt example.
   --  ------------------------------
   procedure Test_Encrypt_Decrypt (T : in out Test) is
      Path1  : constant String := Util.Tests.Get_Test_Path ("copy.aes");
      Path2  : constant String := Util.Tests.Get_Test_Path ("copy.aes.txt");
      Result : UString;
   begin
      if Ada.Directories.Exists (Path1) then
         Ada.Directories.Delete_File (Path1);
      end if;
      if Ada.Directories.Exists (Path2) then
         Ada.Directories.Delete_File (Path2);
      end if;
      T.Execute ("bin/encrypt Makefile secret-key " & Path1, Result);
      T.Execute ("bin/decrypt " & Path1 & " secret-key " & Path2, Result);
      Assert_Equal_Files (T, "Makefile", Path2, "Encrypt+Decrypt failed");
   end Test_Encrypt_Decrypt;

   --  ------------------------------
   --  Tests the lzma_encrypt/lzma_decrypt example.
   --  ------------------------------
   procedure Test_Lzma_Encrypt_Decrypt (T : in out Test) is
      Path1  : constant String := Util.Tests.Get_Test_Path ("copy.aes.xz");
      Path2  : constant String := Util.Tests.Get_Test_Path ("copy.aes.xz.txt");
      Result : UString;
   begin
      if Ada.Directories.Exists (Path1) then
         Ada.Directories.Delete_File (Path1);
      end if;
      if Ada.Directories.Exists (Path2) then
         Ada.Directories.Delete_File (Path2);
      end if;
      T.Execute ("bin/lzma_encrypt Makefile secret-key " & Path1, Result);
      T.Execute ("bin/lzma_decrypt " & Path1 & " secret-key " & Path2, Result);
      Assert_Equal_Files (T, "Makefile", Path2, "LZMA+Encrypt+Decrypt failed");
   end Test_Lzma_Encrypt_Decrypt;

   --  ------------------------------
   --  Tests the lzma_encrypt_b64/lzma_decrypt_b6 example.
   --  ------------------------------
   procedure Test_Lzma_Encrypt_Decrypt_B64 (T : in out Test) is
      Path1  : constant String := Util.Tests.Get_Test_Path ("copy.aes.xz.b64");
      Path2  : constant String := Util.Tests.Get_Test_Path ("copy.aes.xz.b64.txt");
      Result : UString;
   begin
      if Ada.Directories.Exists (Path1) then
         Ada.Directories.Delete_File (Path1);
      end if;
      if Ada.Directories.Exists (Path2) then
         Ada.Directories.Delete_File (Path2);
      end if;
      T.Execute ("bin/lzma_encrypt_b64 Makefile secret-key " & Path1, Result);
      T.Execute ("bin/lzma_decrypt_b64 " & Path1 & " secret-key " & Path2, Result);
      Assert_Equal_Files (T, "Makefile", Path2, "LZMA+Encrypt+Decrypt_B64 failed");
   end Test_Lzma_Encrypt_Decrypt_B64;

   --  ------------------------------
   --  Tests the multipart example.
   --  ------------------------------
   procedure Test_Multipart (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
   begin
      Util.Processes.Tools.Execute ("bin/multipart samples/ISRG_Root_X1.pem", List, Status);
      Assert_Equals (T, 0, Status, "Invalid execution status");
      Assert_Equals (T, 29, Natural (List.Length), "Invalid number of lines");
      Assert (T, List.Contains ("MIIFazCCA1OgAwIBAgIRAIIQz7DSQON"
                                & "ZRGPgu2OCiwAwDQYJKoZIhvcNAQELBQAw"),
              "Expected first line not found");
      Assert (T, List.Contains ("emyPxgcYxn/eR44/KJ4EBs+lVDR3vey"
                                & "Jm+kXQ99b21/+jh5Xos1AnX5iItreGCc="),
              "Expected last line not found");
   end Test_Multipart;

   --  ------------------------------
   --  Tests the dumpcert example.
   --  ------------------------------
   procedure Test_Dumpcert (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
   begin
      Util.Processes.Tools.Execute ("bin/dumpcert samples/ISRG_Root_X1.pem", List, Status);
      Assert_Equals (T, 0, Status, "Invalid execution status");
      Assert_Equals (T, 47, Natural (List.Length), "Invalid number of lines");
      Assert (T, List.Contains ("      Tag: OID len  9 value: 1.2.840.113549.1.1.11"),
              "Expected line not found");
      Assert (T, List.Contains ("          Tag: PRINTABLESTRING len  32 value:"
                                & " Internet Security Research Group"),
              "Expected line not found");
      Assert (T, List.Contains ("    Tag: OID len  9 value: 1.2.840.113549.1.1.11"),
              "Expected line not found");
      Assert (T, List.Contains ("  Tag: BITSTRING Len  513"),
              "Expected line not found");
   end Test_Dumpcert;

   --  ------------------------------
   --  Tests the sha256 example.
   --  ------------------------------
   procedure Test_Sha256 (T : in out Test) is
      Result : UString;
   begin
      T.Execute ("bin/sha256 samples/ISRG_Root_X1.pem", Result);
      Assert_Matches (T, "22b557a27055b33606b6559f37703928d3e4ad79f110b407d04986e1843543d1",
                      Result);
   end Test_Sha256;

   --  ------------------------------
   --  Tests the env example.
   --  ------------------------------
   procedure Test_Env (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
   begin
      Util.Processes.Tools.Execute ("bin/env", List, Status);
      Assert_Equals (T, 0, Status, "Invalid execution status");
      Assert (T, List.Contains ("ENV_VAR=test1"),
              "Expected line not found");
   end Test_Env;

   --  ------------------------------
   --  Tests the serialize example.
   --  ------------------------------
   procedure Test_Serialize (T : in out Test) is
      Result : UString;
   begin
      T.Execute ("bin/serialize", Result);
      Assert_Matches (T, "{""person"":{""name"":""Harry Potter"""
                      & ",""gender"":""male"",""age"": 17}}",
                      Result);
   end Test_Serialize;

   --  ------------------------------
   --  Tests the serialize_xml example.
   --  ------------------------------
   procedure Test_Serialize_XML (T : in out Test) is
      Result : UString;
   begin
      T.Execute ("bin/serialize_xml", Result);
      Assert_Matches (T, "<person><name>Harry Potter</name>"
                      & "<gender>male</gender><age>17</age></person>",
                      Result);
   end Test_Serialize_XML;

   --  ------------------------------
   --  Tests the proplist example.
   --  ------------------------------
   procedure Test_Proplist (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
   begin
      Util.Processes.Tools.Execute ("bin/proplist samples/test.ini", List, Status);
      Assert_Equals (T, 0, Status, "Invalid execution status");
      Assert (T, List.Contains ("user=mysql"),
              "Expected line not found");
      Assert (T, List.Contains ("[XML::SAX::Expat]"),
              "Expected line not found");
      Assert (T, List.Contains ("http://xml.org/sax/features/namespaces=1"),
              "Expected line not found");
   end Test_Proplist;

   --  ------------------------------
   --  Tests the properties example.
   --  ------------------------------
   procedure Test_Properties (T : in out Test) is
      List   : Util.Strings.Vectors.Vector;
      Status : Integer;
      Expect : constant Util.Strings.Vectors.Vector :=
         "test.count =  20"
       & "No property: 'test.repeat'";
   begin
      Util.Processes.Tools.Execute ("bin/properties", List, Status);
      Assert_Equals (T, 0, Status, "Invalid execution status");
      Assert_Equal_Vectors (T, Expect, List, "invalid log output");
   end Test_Properties;

end Util.Samples_Tests;
