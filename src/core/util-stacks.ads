-----------------------------------------------------------------------
--  util-stacks -- Simple stack
--  Copyright (C) 2010, 2011, 2013, 2022 Stephane Carrez
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
with Ada.Finalization;
generic
   type Element_Type is private;
   type Element_Type_Access is access all Element_Type;
package Util.Stacks is

   pragma Preelaborate;

   type Stack is limited private;

   --  Get access to the current stack element.
   function Current (Container : in Stack) return Element_Type_Access;

   --  Push an element on top of the stack making the new element the current one.
   procedure Push (Container : in out Stack);

   --  Pop the top element.
   procedure Pop (Container : in out Stack);

   --  Clear the stack.
   procedure Clear (Container : in out Stack);

   --  Returns true if the stack is empty.
   function Is_Empty (Container : in out Stack) return Boolean;

   --  Get the access to the stack element at the given position.
   function Get (Container : in Stack;
                 Position  : in Positive) return Element_Type_Access;

   type Element_Type_Array is array (Natural range <>) of aliased Element_Type;

   procedure Read (Container : in Stack;
                   Process   : not null
                     access procedure (Content : in Element_Type_Array));

private
   type Element_Type_Array_Access is access all Element_Type_Array;

   type Stack is new Ada.Finalization.Limited_Controlled with record
      Current : Element_Type_Access := null;
      Stack   : Element_Type_Array_Access := null;
      Pos     : Natural := 0;
   end record;

   --  Release the stack
   overriding
   procedure Finalize (Obj : in out Stack);

end Util.Stacks;
