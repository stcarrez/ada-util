-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Assertions is

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
   procedure Assert_Equals_T (T       : in AUnit.Assertions.Test'Class;
                              Expect  : in Value_Type;
                              Value   : in Value_Type;
                              Message : in String := "Test failed";
                              Source  : in String := GNAT.Source_Info.File;
                              Line    : in Natural := GNAT.Source_Info.Line) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (Condition => Expect = Value,
                               Message   => Message & ": expecting '"
                               & Value_Type'Image (Expect) & "'"
                               & " value was '"
                               & Value_Type'Image (Value) & "'",
                               Source    => Source,
                               Line      => Line);
   end Assert_Equals_T;

end Util.Assertions;
