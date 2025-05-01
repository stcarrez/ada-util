-----------------------------------------------------------------------
--  util-stacks -- Simple stack
--  Copyright (C) 2010, 2011, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

package body Util.Stacks is

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Type_Array,
                                     Element_Type_Array_Access);

   --  ------------------------------
   --  Get access to the current stack element.
   --  ------------------------------
   function Current (Container : in Stack) return Element_Type_Access is
   begin
      return Container.Current;
   end Current;

   function Top (Container : in Stack) return Element_Type is
   begin
      return Container.Current.all;
   end Top;

   --  ------------------------------
   --  Push an element on top of the stack making the new element the current one.
   --  ------------------------------
   procedure Push (Container : in out Stack) is
   begin
      if Container.Stack = null then
         Container.Stack := new Element_Type_Array (1 .. 100);
         Container.Pos := Container.Stack'First;
      elsif Container.Pos = Container.Stack'Last then
         declare
            Old : Element_Type_Array_Access := Container.Stack;
         begin
            Container.Stack := new Element_Type_Array (1 .. Old'Last + 100);
            Container.Stack (1 .. Old'Last) := Old (1 .. Old'Last);
            Free (Old);
         end;
      end if;
      if Container.Pos /= Container.Stack'First then
         Container.Stack (Container.Pos + 1) := Container.Stack (Container.Pos);
      end if;
      Container.Pos := Container.Pos + 1;
      Container.Current := Container.Stack (Container.Pos)'Access;
   end Push;

   procedure Push (Container : in out Stack;
                   Element   : in Element_Type) is
   begin
      Push (Container);
      Container.Current.all := Element;
   end Push;

   --  ------------------------------
   --  Pop the top element.
   --  ------------------------------
   procedure Pop (Container : in out Stack) is
   begin
      if Container.Pos > Container.Stack'First then
         Container.Pos := Container.Pos - 1;
         Container.Current   := Container.Stack (Container.Pos)'Access;
      else
         Container.Current := null;
      end if;
   end Pop;

   --  ------------------------------
   --  Clear the stack.
   --  ------------------------------
   procedure Clear (Container : in out Stack) is
   begin
      if Container.Stack /= null then
         Container.Pos := Container.Stack'First;
      end if;
      Container.Current := null;
   end Clear;

   --  ------------------------------
   --  Returns true if the stack is empty.
   --  ------------------------------
   function Is_Empty (Container : in Stack) return Boolean is
   begin
      return Container.Stack = null or else Container.Pos = Container.Stack'First;
   end Is_Empty;

   --  ------------------------------
   --  Get the access to the stack element at the given position.
   --  ------------------------------
   function Get (Container : in Stack;
                 Position  : in Positive) return Element_Type_Access is
   begin
      if Container.Stack = null or else Container.Pos > Position then
         return null;
      else
         return Container.Stack (Position)'Access;
      end if;
   end Get;

   procedure Read (Container : in Stack;
                   Process   : not null
                     access procedure (Content : in Element_Type_Array)) is
      Stack : constant Element_Type_Array_Access := Container.Stack;
   begin
      if Stack /= null then
         Process (Stack (Stack'First + 1 .. Container.Pos));
      end if;
   end Read;

   --  ------------------------------
   --  Release the stack
   --  ------------------------------
   overriding
   procedure Finalize (Obj : in out Stack) is
   begin
      Free (Obj.Stack);
   end Finalize;

end Util.Stacks;
