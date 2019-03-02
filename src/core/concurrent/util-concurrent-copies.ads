-----------------------------------------------------------------------
--  util-concurrent-copies -- Protect concurrent read/writes of a variable
--  Copyright (C) 2011, 2017 Stephane Carrez
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
