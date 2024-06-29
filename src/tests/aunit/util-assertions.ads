-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AUnit.Assertions;

with GNAT.Source_Info;

package Util.Assertions is

   --  Check that the value matches what we expect.
   generic
      type Value_Type is (<>);
   procedure Assert_Equals_T (T       : in AUnit.Assertions.Test'Class;
                              Expect  : in Value_Type;
                              Value   : in Value_Type;
                              Message : in String := "Test failed";
                              Source  : in String := GNAT.Source_Info.File;
                              Line    : in Natural := GNAT.Source_Info.Line);

end Util.Assertions;
