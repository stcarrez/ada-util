-----------------------------------------------------------------------
--  util-encodes-ecc-tests - ECC function tests
--  Copyright (C) 2019 Stephane Carrez
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
with Interfaces;
with Util.Test_Caller;

package body Util.Encoders.ECC.Tests is

   use Interfaces;

   package Caller is new Util.Test_Caller (Test, "Encoders.ECC");

   procedure Assert_Equals (T       : in out Test;
                            Expect  : in ECC_Code;
                            Code    : in ECC_Code;
                            Message : in String);

   procedure Test_ECC_Block (T      : in out Test;
                             Data   : in Ada.Streams.Stream_Element_Array;
                             Expect : in ECC_Code;
                             Title  : in String);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Encoders.ECC.Make+Check (256)",
                       Test_ECC_Block_256'Access);
      Caller.Add_Test (Suite, "Test Util.Encoders.ECC.Make+Check (512)",
                       Test_ECC_Block_512'Access);
   end Add_Tests;

   procedure Assert_Equals (T       : in out Test;
                            Expect  : in ECC_Code;
                            Code    : in ECC_Code;
                            Message : in String) is
   begin
      Util.Tests.Assert_Equals (T, Natural (Expect (0)), Natural (Code (0)),
                                "ECC(0): " & Message);
      Util.Tests.Assert_Equals (T, Natural (Expect (1)), Natural (Code (1)),
                                "ECC(1): " & Message);
      Util.Tests.Assert_Equals (T, Natural (Expect (2)), Natural (Code (2)),
                                "ECC(2): " & Message);
   end Assert_Equals;

   --  ------------------------------
   --  Test ECC correction on 256 bytes block.
   --  ------------------------------
   procedure Test_ECC_Block (T      : in out Test;
                             Data   : in Ada.Streams.Stream_Element_Array;
                             Expect : in ECC_Code;
                             Title  : in String) is
      Data2  : Ada.Streams.Stream_Element_Array := Data;
      Code1  : ECC_Code;
      Code2  : ECC_Code;
      Code3  : ECC_Code;
      Result : ECC_Result;
   begin
      Make (Data, Code1);

      Assert_Equals (T, Expect, Code1, Title);

      for I in Data'Range loop
         for J in 0 .. 7 loop
            Data2 (I) := Data2 (I) xor Stream_Element (Shift_Left (Unsigned_8 (1), J));
            Make (Data2, Code2);

            Result := Correct (Data2, Expect);
            T.Assert (Result = CORRECTABLE_ERROR, Title & ": not corrected"
                     & Natural'Image (Natural (I)) & " bit" & Natural'Image (J));

            Make (Data2, Code3);
            Assert_Equals (T, Expect, Code3, Title & ": bad ECC"
                           & Natural'Image (Natural (I)) & " bit" & Natural'Image (J));
            T.Assert (Data = Data2, Title & ": invalid data block");
         end loop;
      end loop;

   end Test_ECC_Block;

   --  ------------------------------
   --  Test ECC correction on 256 bytes block.
   --  ------------------------------
   procedure Test_ECC_Block_256 (T : in out Test) is
      Data   : Ada.Streams.Stream_Element_Array (0 .. 255) := (others => 0);
   begin
      Test_ECC_Block (T, Data, (16#ff#, 16#ff#, 16#ff#), "Zero block");

      Data (45) := 16#20#;
      Test_ECC_Block (T, Data, (16#59#, 16#a6#, 16#67#), "Zero block, @45=20");

      Data (45) := 16#10#;
      Test_ECC_Block (T, Data, (16#59#, 16#a6#, 16#6b#), "Zero block, @45=10");

      Data (45) := 16#41#;
      Test_ECC_Block (T, Data, (16#ff#, 16#ff#, 16#0f#), "Zero block, @45=41");

   end Test_ECC_Block_256;

   --  ------------------------------
   --  Test ECC correction on 512 bytes block.
   --  ------------------------------
   procedure Test_ECC_Block_512 (T : in out Test) is
      Data   : Ada.Streams.Stream_Element_Array (0 .. 511) := (others => 0);
   begin
      Test_ECC_Block (T, Data, (16#ff#, 16#ff#, 16#ff#), "Zero block 512");

      Data (45) := 16#20#;
      Test_ECC_Block (T, Data, (16#59#, 16#a6#, 16#66#), "Zero block 512, @45=20");

      Data (45) := 16#10#;
      Test_ECC_Block (T, Data, (16#59#, 16#a6#, 16#6a#), "Zero block 512, @45=10");

      Data (45) := 16#41#;
      Test_ECC_Block (T, Data, (16#ff#, 16#ff#, 16#0f#), "Zero block 512, @45=41");

   end Test_ECC_Block_512;

end Util.Encoders.ECC.Tests;
