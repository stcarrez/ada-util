-----------------------------------------------------------------------
--  util-serialize-contexts -- Contexts for serialization framework
--  Copyright (C) 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Util.Concurrent.Counters;
package body Util.Serialize.Contexts is

   procedure Free is new Ada.Unchecked_Deallocation (Data'Class, Data_Access);

   --  ------------------------------
   --  Context data key
   --  ------------------------------
   Next_Key : Util.Concurrent.Counters.Counter := Util.Concurrent.Counters.ONE;

   --  ------------------------------
   --  Allocate a unique data key for a mapper.
   --  ------------------------------
   procedure Allocate (Key : out Data_Key) is
      Val : Integer;
   begin
      Util.Concurrent.Counters.Increment (Next_Key, Val);
      Key := Data_Key (Val);
   end Allocate;

   function Hash (Key : in Data_Key) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Hash;

   --  ------------------------------
   --  Reader context
   --  ------------------------------

   --  ------------------------------
   --  Get the data object associated with the given key.
   --  Raises No_Data exception if there is no data object.
   --  ------------------------------
   function Get_Data (Ctx : in Context;
                      Key : in Data_Key) return Data_Access is
      Pos : constant Data_Map.Cursor := Ctx.Data.Find (Key);
   begin
      if Data_Map.Has_Element (Pos) then
         return Data_Map.Element (Pos);
      else
         raise No_Data;
      end if;
   end Get_Data;

   --  ------------------------------
   --  Set the data object associated with the given key.
   --  ------------------------------
   procedure Set_Data (Ctx     : in out Context;
                       Key     : in Data_Key;
                       Content : in Data_Access) is
      Pos : constant Data_Map.Cursor := Ctx.Data.Find (Key);
   begin
      if Data_Map.Has_Element (Pos) then
         declare
            Old : Data_Access := Data_Map.Element (Pos);
         begin
            if Old = Content then
               return;
            end if;
            Old.Finalize;
            Free (Old);
         end;
         Ctx.Data.Replace_Element (Position => Pos, New_Item => Content);
      else
         Ctx.Data.Insert (Key => Key, New_Item => Content);
      end if;
   end Set_Data;

   --  ------------------------------
   --  Free the context data.
   --  ------------------------------
   overriding
   procedure Finalize (Ctx : in out Context) is
      Pos     : Data_Map.Cursor;
      Content : Data_Access;
   begin
      loop
         Pos := Ctx.Data.First;
         exit when not Data_Map.Has_Element (Pos);
         Content := Data_Map.Element (Pos);
         Content.Finalize;
         Free (Content);
         Ctx.Data.Delete (Pos);
      end loop;
   end Finalize;

end Util.Serialize.Contexts;
