-----------------------------------------------------------------------
--  util-stacks -- Simple stack
--  Copyright (C) 2010, 2011, 2022 Stephane Carrez
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
   function Is_Empty (Container : in out Stack) return Boolean is
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
