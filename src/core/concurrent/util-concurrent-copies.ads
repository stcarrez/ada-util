-----------------------------------------------------------------------
--  util-concurrent-copies -- Protect concurrent read/writes of a variable
--  Copyright (C) 2011, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
generic
   type Element_Type is private;
package Util.Concurrent.Copies is

   pragma Preelaborate;

   --  The <b>Atomic</b> protected type defines an element which can be obtained
   --  and changed atomically.  The default Ada construct:
   --
   --     Val1 := Val2;
   --
   --  does not guarantee atomicity of the copy (assignment) and the possible
   --  invocation of the <b>Adjust</b> operation on Val1.
   --  Atomicity is achieved by this protected type through the <b>Get</b> and <b>Set</b>
   --  methods.
   protected type Atomic is

      --  Get the value atomically
      function Get return Element_Type;

      --  Change the value atomically.
      procedure Set (Object : in Element_Type);

   private
      Value : Element_Type;
   end Atomic;

end Util.Concurrent.Copies;
