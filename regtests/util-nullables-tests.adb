-----------------------------------------------------------------------
--  util-nullables-tests - Test for nullables types
--  Copyright (C) 2021 Stephane Carrez
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
with Util.Properties;
package body Util.Nullables.Tests is

   use Util.Properties; --  used for "+" operator

   package Caller is new Util.Test_Caller (Test, "Nullables");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Nullables.Driver.Execute",
                       Test_Nullables'Access);
   end Add_Tests;

   procedure Test_Nullables (T : in out Test) is
   begin
      declare
         B1    : Nullable_Boolean := (Value => True, Is_Null => False);
         B2    : Nullable_Boolean := (Value => False, Is_Null => False);
      begin
         --  Check boolean
         T.Assert (B1 /= B2, "Bad Nullable_Boolean comparison");
         T.Assert (B1 /= Null_Boolean, "Bad Nullable_Boolean comparison");
         B1.Value := B2.Value;
         T.Assert (B1 = B2, "Bad Nullable_Boolean comparison");
         B1.Value := not B1.Value;
         B1.Is_Null := True;
         T.Assert (B1 = Null_Boolean, "Bad Nullable_Boolean comparison");
         B2.Is_Null := True;
         T.Assert (B2 = Null_Boolean, "Bad Nullable_Boolean comparison");
         T.Assert (B2 = B1, "Bad Nullable_Boolean comparison");
      end;

      declare
         I1    : Nullable_Integer := (Value => 0, Is_Null => False);
         I2    : Nullable_Integer := (Value => 12, Is_Null => False);
      begin
         --  Check integer
         T.Assert (I1 /= I2, "Bad Nullable_Integer comparison");
         T.Assert (I1 /= Null_Integer, "Bad Nullable_Integer comparison");
         I1.Value := I2.Value;
         T.Assert (I1 = I2, "Bad Nullable_Integer comparison");
         I1.Value := 44;
         I1.Is_Null := True;
         T.Assert (I1 = Null_Integer, "Bad Nullable_Integer comparison");
         I2.Is_Null := True;
         T.Assert (I2 = Null_Integer, "Bad Nullable_Integer comparison");
         T.Assert (I2 = I1, "Bad Nullable_Integer comparison");
      end;

      declare
         L1    : Nullable_Long := (Value => 0, Is_Null => False);
         L2    : Nullable_Long := (Value => 123, Is_Null => False);
      begin
         --  Check long
         T.Assert (L1 /= L2, "Bad Nullable_Long comparison");
         T.Assert (L1 /= Null_Long, "Bad Nullable_Long comparison");
         L1.Value := L2.Value;
         T.Assert (L1 = L2, "Bad Nullable_Long comparison");
         L1.Value := 12;
         L1.Is_Null := True;
         T.Assert (L1 = Null_Long, "Bad Nullable_Long comparison");
         L2.Is_Null := True;
         T.Assert (L2 = Null_Long, "Bad Nullable_Long comparison");
         T.Assert (L2 = L1, "Bad Nullable_Long comparison");
      end;

      declare
         S1    : Nullable_String := (Value => +("a"), Is_Null => False);
         S2    : Nullable_String := (Value => +("b"), Is_Null => False);
      begin
         --  Check string
         T.Assert (S1 /= S2, "Bad Nullable_String comparison");
         T.Assert (S1 /= Null_String, "Bad Nullable_String comparison");
         S1.Value := S2.Value;
         T.Assert (S1 = S2, "Bad Nullable_String comparison");
         S1.Value := +("c");
         S1.Is_Null := True;
         T.Assert (S1 = Null_String, "Bad Nullable_String comparison");
         S2.Is_Null := True;
         T.Assert (S2 = Null_String, "Bad Nullable_String comparison");
         T.Assert (S2 = S1, "Bad Nullable_String comparison");
      end;

      declare
         TEST_TIME : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year    => 2000,
                                                                         Month   => 11,
                                                                         Day     => 17,
                                                                         Seconds => 0.0);
         T1    : Nullable_Time := (Value => TEST_TIME, Is_Null => False);
         T2    : Nullable_Time := (Value => Ada.Calendar.Clock, Is_Null => False);
      begin
         --  Check string
         T.Assert (T1 /= T2, "Bad Nullable_Time comparison");
         T.Assert (T1 /= Null_Time, "Bad Nullable_Time comparison");
         T1.Value := T2.Value;
         T.Assert (T1 = T2, "Bad Nullable_Time comparison");
         T1.Value := TEST_TIME;
         T1.Is_Null := True;
         T.Assert (T1 = Null_Time, "Bad Nullable_Time comparison");
         T2.Is_Null := True;
         T.Assert (T2 = Null_Time, "Bad Nullable_Time comparison");
         T.Assert (T2 = T1, "Bad Nullable_Time comparison");
      end;

   end Test_Nullables;

end Util.Nullables.Tests;
