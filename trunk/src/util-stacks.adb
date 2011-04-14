-----------------------------------------------------------------------
--  util-stacks -- Simple stack
--  Copyright (C) 2010, 2011 Stephane Carrez
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
      Container.Pos := Container.Pos - 1;
      Container.Current   := Container.Stack (Container.Pos)'Access;
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
   --  Release the stack
   --  ------------------------------
   overriding
   procedure Finalize (Obj : in out Stack) is
   begin
      Free (Obj.Stack);
   end Finalize;

end Util.Stacks;
