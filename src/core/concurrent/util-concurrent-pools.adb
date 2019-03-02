-----------------------------------------------------------------------
--  util-concurrent-pools -- Concurrent Pools
--  Copyright (C) 2011, 2014, 2015, 2017 Stephane Carrez
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
package body Util.Concurrent.Pools is

   --  ------------------------------
   --  Get an element instance from the pool.
   --  Wait until one instance gets available.
   --  ------------------------------
   procedure Get_Instance (From : in out Pool;
                           Item : out Element_Type;
                           Wait : in Duration := FOREVER) is
   begin
      if Wait < 0.0 then
         From.List.Get_Instance (Item);
      else
         select
            From.List.Get_Instance (Item);
         or
            delay Wait;
            raise Timeout;
         end select;
      end if;
   end Get_Instance;

   --  ------------------------------
   --  Put the element back to the pool.
   --  ------------------------------
   procedure Release (Into : in out Pool;
                      Item : in Element_Type) is
   begin
      Into.List.Release (Item);
   end Release;

   --  ------------------------------
   --  Set the pool size.
   --  ------------------------------
   procedure Set_Size (Into : in out Pool;
                       Capacity : in Positive) is
   begin
      Into.List.Set_Size (Capacity);
   end Set_Size;

   --  ------------------------------
   --  Get the number of available elements in the pool.
   --  ------------------------------
   procedure Get_Available (From      : in out Pool;
                            Available : out Natural) is
   begin
      Available := From.List.Get_Available;
   end Get_Available;

   --  ------------------------------
   --  Release the pool elements.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Pool) is
   begin
      Object.List.Set_Size (0);
   end Finalize;

   --  Pool of objects
   protected body Protected_Pool is

      --  ------------------------------
      --  Get an element instance from the pool.
      --  Wait until one instance gets available.
      --  ------------------------------
      entry Get_Instance (Item : out Element_Type) when Available > 0 is
      begin
         Item := Elements (Available);
         Available := Available - 1;
      end Get_Instance;

      --  ------------------------------
      --  Put the element back to the pool.
      --  ------------------------------
      procedure Release (Item : in Element_Type) is
      begin
         Available := Available + 1;
         Elements (Available) := Item;
      end Release;

      --  ------------------------------
      --  Set the pool size.
      --  ------------------------------
      procedure Set_Size (Capacity : in Natural) is
         procedure Free is new Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);
      begin
         if Capacity = 0 then
            Free (Elements);
         elsif Elements = null then
            Elements := new Element_Array (1 .. Capacity);
         else
            declare
               New_Array : constant Element_Array_Access := new Element_Array (1 .. Capacity);
            begin
               if Capacity > Elements'Size then
                  New_Array (1 .. Elements'Last) := Elements (1 .. Elements'Last);
               else
                  New_Array (1 .. Capacity) := Elements (1 .. Capacity);
               end if;

               Free (Elements);
               Elements := New_Array;
            end;
         end if;
      end Set_Size;

      --  ------------------------------
      --  Get the number of available elements.
      --  ------------------------------
      function Get_Available return Natural is
      begin
         return Available;
      end Get_Available;

   end Protected_Pool;

end Util.Concurrent.Pools;
