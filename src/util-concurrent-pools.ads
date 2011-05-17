-----------------------------------------------------------------------
--  Util.Concurrent.Pools -- Concurrent Pools
--  Copyright (C) 2011 Stephane Carrez
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

--  The <b>Util.Concurrent.Pools</b> generic defines a pool of objects which
--  can be shared by multiple threads.  First, the pool is configured to have
--  a number of objects by using the <b>Set_Size</b> procedure.  Then, a thread
--  that needs an object uses the <b>Get_Instance</b> to get an object.
--  The object is removed from the pool.  As soon as the thread has finished,
--  it puts back the object in the pool using the <b>Release</b> procedure.
--
--  The <b>Get_Instance</b> entry will block until an object is available.
generic
   type Element_Type is private;
package Util.Concurrent.Pools is

   type Element_Array_Access is private;

   Null_Element_Array : constant Element_Array_Access;

   --  Pool of objects
   protected type Pool is

      --  Get an element instance from the pool.
      --  Wait until one instance gets available.
      entry Get_Instance (Item : out Element_Type);

      --  Put the element back to the pool.
      procedure Release (Item : in Element_Type);

      --  Set the pool size.
      procedure Set_Size (Capacity : in Positive);

   private
      Available     : Natural := 0;
      Elements      : Element_Array_Access := Null_Element_Array;
   end Pool;

private
   type Element_Array is array (Positive range <>) of Element_Type;
   type Element_Array_Access is access all Element_Array;

   Null_Element_Array : constant Element_Array_Access := null;
end Util.Concurrent.Pools;
