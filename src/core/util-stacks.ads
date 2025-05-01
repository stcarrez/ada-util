-----------------------------------------------------------------------
--  util-stacks -- Simple stack
--  Copyright (C) 2010, 2011, 2013, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Finalization;
generic
   type Element_Type is private;
   type Element_Type_Access is access all Element_Type;
package Util.Stacks is

   pragma Preelaborate;

   type Stack is tagged limited private;

   --  Get access to the current stack element.
   function Current (Container : in Stack) return Element_Type_Access;
   function Top (Container : in Stack) return Element_Type;

   --  Push an element on top of the stack making the new element the current one.
   procedure Push (Container : in out Stack);
   procedure Push (Container : in out Stack;
                   Element   : in Element_Type);

   --  Pop the top element.
   procedure Pop (Container : in out Stack);

   --  Clear the stack.
   procedure Clear (Container : in out Stack);

   --  Returns true if the stack is empty.
   function Is_Empty (Container : in Stack) return Boolean;

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
