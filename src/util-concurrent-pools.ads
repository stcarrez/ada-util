-----------------------------------------------------------------------
--  util-concurrent-pools -- Concurrent Pools
--  Copyright (C) 2011, 2015, 2018 Stephane Carrez
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

   pragma Preelaborate;

   FOREVER : constant Duration := -1.0;

   --  Exception raised if the Get_Instance timeout exceeded.
   Timeout : exception;

   --  Pool of objects
   type Pool is limited new Ada.Finalization.Limited_Controlled with private;

   --  Get an element instance from the pool.
   --  Wait until one instance gets available.
   procedure Get_Instance (From : in out Pool;
                           Item : out Element_Type;
                           Wait : in Duration := FOREVER);

   --  Put the element back to the pool.
   procedure Release (Into : in out Pool;
                      Item : in Element_Type);

   --  Set the pool size.
   procedure Set_Size (Into     : in out Pool;
                       Capacity : in Positive);

   --  Get the number of available elements in the pool.
   procedure Get_Available (From      : in out Pool;
                            Available : out Natural);

   --  Release the pool elements.
   overriding
   procedure Finalize (Object : in out Pool);

private

   --  To store the pool elements, we use an array which is allocated dynamically
   --  by the <b>Set_Size</b> protected operation.  The generated code is smaller
   --  compared to the use of Ada vectors container.
   type Element_Array is array (Positive range <>) of Element_Type;
   type Element_Array_Access is access all Element_Array;

   Null_Element_Array : constant Element_Array_Access := null;

   --  Pool of objects
   protected type Protected_Pool is

      --  Get an element instance from the pool.
      --  Wait until one instance gets available.
      entry Get_Instance (Item : out Element_Type);

      --  Put the element back to the pool.
      procedure Release (Item : in Element_Type);

      --  Set the pool size.
      procedure Set_Size (Capacity : in Natural);

      --  Get the number of available elements.
      function Get_Available return Natural;

   private
      Available     : Natural := 0;
      Elements      : Element_Array_Access := Null_Element_Array;
   end Protected_Pool;

   type Pool is limited new Ada.Finalization.Limited_Controlled with record
      List : Protected_Pool;
   end record;

end Util.Concurrent.Pools;
