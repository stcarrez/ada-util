-----------------------------------------------------------------------
--  util-serialize-contexts -- Contexts for serialization framework
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
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Finalization;
package Util.Serialize.Contexts is

   No_Data : exception;

   --  ------------------------------
   --  Context data key
   --  ------------------------------
   type Data_Key is private;

   --  Allocate a unique data key for a mapper.
   procedure Allocate (Key : out Data_Key);

   --  ------------------------------
   --  Data context for the mapper
   --  ------------------------------
   type Data is tagged limited private;
   type Data_Access is access all Data'Class;

   --  Finalize the data object when it is removed from the reader context.
   procedure Finalize (Object : in out Data) is null;

   --  ------------------------------
   --  Reader context
   --  ------------------------------
   type Context is new Ada.Finalization.Limited_Controlled with private;

   --  Get the data object associated with the given key.
   --  Raises No_Data exception if there is no data object.
   function Get_Data (Ctx : in Context;
                      Key : in Data_Key) return Data_Access;

   --  Set the data object associated with the given key.
   --  Free the previous data object if there was one.
   --  The data object will be freed if the context is destroyed.
   procedure Set_Data (Ctx     : in out Context;
                       Key     : in Data_Key;
                       Content : in Data_Access);

private

   type Data_Key is new Integer;

   function Hash (Key : in Data_Key) return Ada.Containers.Hash_Type;

   type Data is tagged limited null record;

   package Data_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Data_Key,
      Element_Type    => Data_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Context is new Ada.Finalization.Limited_Controlled with record
      Data : Data_Map.Map;
   end record;

   --  Free the context data.
   overriding
   procedure Finalize (Ctx : in out Context);

end Util.Serialize.Contexts;
