-----------------------------------------------------------------------
--  util-xunit - Unit tests on top of AUnit
--  Copyright (C) 2009, 2010, 2011, 2012, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Assertions;

with Util.Tests.Reporter;
package body Util.XUnit is

   procedure Assert (T         : in Test_Case;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : in String := GNAT.Source_Info.File;
                     Line      : in Natural := GNAT.Source_Info.Line) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (Condition, Message, Source, Line);
   end Assert;

   procedure Assert (T         : in Test;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : in String := GNAT.Source_Info.File;
                     Line      : in Natural := GNAT.Source_Info.Line) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (Condition, Message, Source, Line);
   end Assert;

   --  ------------------------------
   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   --  ------------------------------
   procedure Harness (Output : in String;
                      XML    : in Boolean;
                      Label  : in String;
                      Result : out Status) is
      pragma Unreferenced (Label);

      function Runner is new AUnit.Run.Test_Runner_With_Status (Suite);

      O  : AUnit.Options.AUnit_Options := AUnit.Options.Default_Options;
   begin
      O.Global_Timer    := True;
      O.Test_Case_Timer := True;
      if XML then
         declare
            Reporter : Util.Tests.Reporter.XML_Reporter;
         begin
            Reporter.File := Ada.Strings.Unbounded.To_Unbounded_String (Output);
            Result := Runner (Reporter, O);
         end;
      else
         declare
            Reporter : AUnit.Reporter.Text.Text_Reporter;
         begin
            Result := Runner (Reporter, O);
         end;
      end if;
   end Harness;

end Util.XUnit;
