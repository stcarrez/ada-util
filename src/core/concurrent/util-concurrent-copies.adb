-----------------------------------------------------------------------
--  util-concurrent-copies -- Concurrent Tools
--  Copyright (C) 2011, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Util.Concurrent.Copies is

   protected body Atomic is
      --  ------------------------------
      --  Get the value atomically
      --  ------------------------------
      function Get return Element_Type is
      begin
         return Value;
      end Get;

      --  ------------------------------
      --  Change the value atomically.
      --  ------------------------------
      procedure Set (Object : in Element_Type) is
      begin
         Value := Object;
      end Set;

   end Atomic;

end Util.Concurrent.Copies;
