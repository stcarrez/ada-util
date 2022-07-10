-----------------------------------------------------------------------
--  Util.Beans.Objects.Discrete_Tests - Generic simple test for discrete object types
--  Copyright (C) 2009, 2010, 2011, 2018, 2021, 2022 Stephane Carrez
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

with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Calendar;
with Ada.Characters.Conversions;
with Util.Test_Caller;
with Util.Beans.Objects.Hash;
package body Util.Beans.Objects.Discrete_Tests is

   use Ada.Strings.Fixed;
   use Ada.Containers;

   procedure Test_Eq (T : Test; V : String; N : Test_Type);
   procedure Test_Conversion (T : Test; V : String; N : Test_Type);
   procedure Test_Lt_Gt (T : Test; V : String; N : Test_Type);
   procedure Test_Le_Ge (T : Test; V : String; N : Test_Type);
   procedure Test_Sub (T : Test; V : String; N : Test_Type);
   procedure Test_Add (T : Test; V : String; N : Test_Type);
   procedure Test_Perf (T : Test; V : String; N : Test_Type);
   procedure Test_Hash (T : in out Test);

   --  Generic test for To_Object and To_XXX types
   --  Several values are specified in the Test_Values string.
   generic
      with procedure Basic_Test (T : in Test; V : String; N : Test_Type);
   procedure Test_Basic_Object (T : in out Test);

   procedure Test_Basic_Object (T : in out Test) is
      pragma Unmodified (T);

      Pos, Next : Natural;
   begin
      Pos := Test_Values'First;
      while Pos <= Test_Values'Last loop
         Next := Index (Test_Values, ",", Pos);
         if Next < Pos then
            Next := Test_Values'Last + 1;
         end if;
         declare
            V : constant String := Test_Values (Pos .. Next - 1);
            N : constant Test_Type := Value (V);
         begin
            Basic_Test (T, V, N);
         end;
         Pos := Next + 1;
      end loop;
   end Test_Basic_Object;

   --  ------------------------------
   --  Test Util.Beans.Objects.To_Object
   --  ------------------------------
   procedure Test_Conversion (T : Test; V : String; N : Test_Type) is
      Value  : Util.Beans.Objects.Object;
   begin
      Value := To_Object (V);
      T.Assert (Condition => To_Type (Value) = N,
                Message   => Test_Name & " returned invalid value: "
                & To_String (Value) & " when we expected: " & V);

      T.Assert (Condition => V = To_String (Value),
                Message   => Test_Name & ".To_String returned invalid value: "
                & To_String (Value) & " when we expected: " & V);

      declare
         Val    : Ada.Strings.Unbounded.Unbounded_String;
         Value2 : Util.Beans.Objects.Object;
      begin
         Val := Ada.Strings.Unbounded.To_Unbounded_String (V);
         Value2 := To_Object (Val);
         T.Assert (Value2 = Value, "Invalid value");

         T.Assert (Condition => To_Type (Value2) = N,
                   Message   => Test_Name & " returned invalid value: "
                     & To_String (Value2) & " when we expected: " & V);
      end;

      declare
         Val    : constant Wide_Wide_String
           := Ada.Characters.Conversions.To_Wide_Wide_String (V);
         Value2 : Util.Beans.Objects.Object;
      begin
         Value2 := To_Object (Val);
         T.Assert (Value2 = Value, "Invalid value");

         T.Assert (Condition => To_Type (Value2) = N,
                   Message   => Test_Name & " returned invalid value: "
                     & To_String (Value2) & " when we expected: " & V);

      end;
   end Test_Conversion;
   procedure Test_To_Object is new Test_Basic_Object (Basic_Test => Test_Conversion);

   --  ------------------------------
   --  Test Util.Beans.Objects.Hash
   --  ------------------------------
   procedure Test_Hash (T : in out Test) is
      pragma Unmodified (T);

      Pos, Next   : Natural;
      Hash_Values : array (Test_Values'Range) of Hash_Type := (others => 0);
      Nb_Hash     : Natural := 0;
   begin
      Pos := Test_Values'First;
      while Pos <= Test_Values'Last loop
         Next := Index (Test_Values, ",", Pos);
         if Next < Pos then
            Next := Test_Values'Last + 1;
         end if;
         declare
            V     : constant String := Test_Values (Pos .. Next - 1);
            N     : constant Test_Type := Value (V);
            Value : constant Util.Beans.Objects.Object := To_Object_Test (N);
            H     : constant Hash_Type := Util.Beans.Objects.Hash (Value);
            Found : Boolean := False;
         begin
            for J in 1 .. Nb_Hash loop
               if Hash_Values (J) = H then
                  Found := True;
               end if;
            end loop;
            if not Found then
               Nb_Hash := Nb_Hash + 1;
               Hash_Values (Nb_Hash) := H;
            end if;
         end;
         Pos := Next + 1;
      end loop;

      Ada.Text_IO.Put_Line ("Found " & Natural'Image (Nb_Hash) & " hash values");
      T.Assert (Nb_Hash > 1, "Only one hash value found");
   end Test_Hash;

   --  ------------------------------
   --  Test Util.Beans.Objects."+"
   --  ------------------------------
   procedure Test_Add (T : Test; V : String; N : Test_Type) is
      Value : Util.Beans.Objects.Object := To_Object_Test (N);
   begin
      Value := Value + To_Object_Test (N);
      T.Assert (Condition => To_Type (Value) = N + N,
                Message   => Test_Name & " returned invalid value: "
                & To_String (Value) & " when we expected: " & V);
   end Test_Add;

   procedure Test_Add is new Test_Basic_Object (Test_Add);

   --  ------------------------------
   --  Test Util.Beans.Objects."-"
   --  ------------------------------
   procedure Test_Sub (T : Test; V : String; N : Test_Type) is
      pragma Unreferenced (V);

      Value : Util.Beans.Objects.Object;
   begin
      Value := To_Object_Test (N) - To_Object_Test (N);
      T.Assert (Condition => To_Type (Value) = N - N,
                Message   => Test_Name & " returned invalid value: "
                & To_String (Value) & " when we expected: 0");
   end Test_Sub;

   procedure Test_Sub is new Test_Basic_Object (Test_Sub);

   --  ------------------------------
   --  Test Util.Beans.Objects."<" and Util.Beans.Objects.">"
   --  ------------------------------
   procedure Test_Lt_Gt (T : Test; V : String; N : Test_Type) is
      Res    : Boolean;
      Is_Neg : constant Boolean := Index (V, "-") = V'First;
      O      : constant Util.Beans.Objects.Object := To_Object_Test (N);
   begin
      Res := To_Object_Test (N) < To_Object_Test (N);
      T.Assert (Condition => Res = False,
                Message   => Test_Name & ".'<' returned invalid value: "
                & Boolean'Image (Res) & " when we expected: false");
      Res := To_Object_Test (N) > To_Object_Test (N);
      T.Assert (Condition => Res = False,
                Message   => Test_Name & ".'>' returned invalid value: "
                & Boolean'Image (Res) & " when we expected: false");
      Res := To_Object_Test (N) + To_Object_Test (N) < To_Object_Test (N);
      T.Assert (Condition => Res = Is_Neg,
                Message   => Test_Name & ".'<' returned invalid value: "
                & Boolean'Image (Res) & " when we expected: "
                & Boolean'Image (Is_Neg)
                & " with value: " & V & "Num=" & Long_Long_Integer'Image (To_Long_Long_Integer (O))
                & " Sum=" & Long_Long_Integer'Image (To_Long_Long_Integer (O + O)));
      Res := To_Object_Test (N) > To_Object_Test (N) + To_Object_Test (N);
      T.Assert (Condition => Res = Is_Neg,
                Message   => Test_Name & ".'>' returned invalid value: "
                & Boolean'Image (Res) & " when we expected: "
                & Boolean'Image (Is_Neg)
                & " with value: " & V);
      if V /= "0" and then V /= "false" and then V /= "true" then
         Res := To_Object_Test (N) < To_Object_Test (N) + To_Object_Test (N);
         T.Assert (Condition => Res = not Is_Neg,
                   Message   => Test_Name & ".'<' returned invalid value: "
                   & Boolean'Image (Res) & " when we expected: "
                   & Boolean'Image (not Is_Neg)
                   & " with value: " & V);
         Res := To_Object_Test (N) + To_Object_Test (N) > To_Object_Test (N);
         T.Assert (Condition => Res = not Is_Neg,
                   Message   => Test_Name & ".'>' returned invalid value: "
                   & Boolean'Image (Res) & " when we expected: "
                   & Boolean'Image (not Is_Neg)
                   & " with value: " & V);
      end if;
   end Test_Lt_Gt;

   --  ------------------------------
   --  Test Util.Beans.Objects."<" and Util.Beans.Objects.">"
   --  ------------------------------
   procedure Test_Le_Ge (T : Test; V : String; N : Test_Type) is
      Res    : Boolean;
      Is_Neg : constant Boolean := Index (V, "-") = V'First;
      O      : constant Util.Beans.Objects.Object := To_Object_Test (N);
   begin
      Res := To_Object_Test (N) <= To_Object_Test (N);
      T.Assert (Condition => Res,
                Message   => Test_Name & ".'<=' returned invalid value: "
                & Boolean'Image (Res) & " when we expected: true");
      Res := To_Object_Test (N) >= To_Object_Test (N);
      T.Assert (Condition => Res,
                Message   => Test_Name & ".'>=' returned invalid value: "
                & Boolean'Image (Res) & " when we expected: true");
      if To_Object_Test (N) + To_Object_Test (N) /= To_Object_Test (N) then
         Res := To_Object_Test (N) + To_Object_Test (N) <= To_Object_Test (N);
         T.Assert (Condition => Res = Is_Neg,
                   Message   => Test_Name & ".'<=' returned invalid value: "
                     & Boolean'Image (Res) & " when we expected: "
                     & Boolean'Image (Is_Neg)
                     & " with value: " & V & "Num="
                     & Long_Long_Integer'Image (To_Long_Long_Integer (O))
                     & " Sum=" & Long_Long_Integer'Image (To_Long_Long_Integer (O + O)));
         Res := To_Object_Test (N) >= To_Object_Test (N) + To_Object_Test (N);
         T.Assert (Condition => Res = Is_Neg,
                   Message   => Test_Name & ".'>' returned invalid value: "
                     & Boolean'Image (Res) & " when we expected: "
                     & Boolean'Image (Is_Neg)
                     & " with value: " & V);
      end if;
      if V /= "0" and then V /= "false" and then V /= "true" then
         Res := To_Object_Test (N) <= To_Object_Test (N) + To_Object_Test (N);
         T.Assert (Condition => Res = not Is_Neg,
                   Message   => Test_Name & ".'<' returned invalid value: "
                   & Boolean'Image (Res) & " when we expected: "
                   & Boolean'Image (not Is_Neg)
                   & " with value: " & V);
         Res := To_Object_Test (N) + To_Object_Test (N) >= To_Object_Test (N);
         T.Assert (Condition => Res = not Is_Neg,
                   Message   => Test_Name & ".'>' returned invalid value: "
                   & Boolean'Image (Res) & " when we expected: "
                   & Boolean'Image (not Is_Neg)
                   & " with value: " & V);
      end if;
   end Test_Le_Ge;

   procedure Test_Lt_Gt is new Test_Basic_Object (Test_Lt_Gt);
   procedure Test_Le_Ge is new Test_Basic_Object (Test_Le_Ge);

   --  ------------------------------
   --  Test Util.Beans.Objects."="
   --  ------------------------------
   procedure Test_Eq (T : Test; V : String; N : Test_Type) is
      Res   : Boolean;
   begin
      Res := To_Object_Test (N) = To_Object_Test (N);
      T.Assert (Condition => Res,
                Message   => Test_Name & ".'=' returned invalid value: "
                & Boolean'Image (Res) & " when we expected: true");

      Res := To_Object_Test (N) = To_Object ("Something" & V);
      T.Assert (Condition => Res = False,
                Message   => Test_Name & ".'=' returned invalid value: "
                & Boolean'Image (Res) & " where we expected: False");
   end Test_Eq;
   procedure Test_Eq is new Test_Basic_Object (Test_Eq);

   --  ------------------------------
   --  Test Util.Beans.Objects."="
   --  ------------------------------
   procedure Test_Perf (T : Test; V : String; N : Test_Type) is
      pragma Unreferenced (T, V);

      use Ada.Calendar;

      Start : Ada.Calendar.Time;
      Value : constant Util.Beans.Objects.Object := To_Object_Test (N);
      D     : Duration;
   begin
      Start := Ada.Calendar.Clock;
      for I in 1 .. 1_000 loop
         declare
            V : Util.Beans.Objects.Object := Value;
         begin
            V := V + V;

            pragma Unreferenced (V);
         end;
      end loop;
      D := Ada.Calendar.Clock - Start;
      Ada.Text_IO.Put_Line ("Perf " & Test_Name & ": " & Duration'Image (D * 1000.0));
   end Test_Perf;
   procedure Test_Perf is new Test_Basic_Object (Test_Perf);

   package Caller is new Util.Test_Caller (Test, "Beans.Objects." & Test_Name);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.To_Object." & Test_Name,
                       Test_To_Object'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.To_String." & Test_Name,
                       Test_To_Object'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.'='." & Test_Name,
                       Test_Eq'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.'+'." & Test_Name,
                       Test_Add'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.'-'." & Test_Name,
                       Test_Sub'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.'<'." & Test_Name,
                       Test_Lt_Gt'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.'>'." & Test_Name,
                       Test_Lt_Gt'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.'<='." & Test_Name,
                       Test_Le_Ge'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.'>='." & Test_Name,
                       Test_Le_Ge'Access);
      Caller.Add_Test (Suite, "Performance Util.Beans.Objects.'>'." & Test_Name,
                       Test_Perf'Access);
      Caller.Add_Test (Suite, "Test Util.Beans.Objects.Hash." & Test_Name,
                       Test_Hash'Access);
   end Add_Tests;

end Util.Beans.Objects.Discrete_Tests;
