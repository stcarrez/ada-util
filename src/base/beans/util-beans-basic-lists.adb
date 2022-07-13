-----------------------------------------------------------------------
--  util-beans-basic-lists -- List bean given access to a vector
--  Copyright (C) 2011, 2013, 2017, 2022 Stephane Carrez
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

package body Util.Beans.Basic.Lists is

   --  ------------------------------
   --  Initialize the list bean.
   --  ------------------------------
   overriding
   procedure Initialize (Object : in out List_Bean) is
      Bean : constant Readonly_Bean_Access := Object.Current'Unchecked_Access;
   begin
      Object.Row := Util.Beans.Objects.To_Object (Bean, Util.Beans.Objects.STATIC);
   end Initialize;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : in List_Bean) return Natural is
   begin
      return Natural (Vectors.Length (From.List));
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out List_Bean;
                            Index : in Natural) is
   begin
      From.Current_Index := Index;
      From.Current := Vectors.Element (From.List, Index);
   end Set_Row_Index;

   --  ------------------------------
   --  Returns the current row index.
   --  ------------------------------
   function Get_Row_Index (From : in List_Bean) return Natural is
   begin
      return From.Current_Index;
   end Get_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From : in List_Bean) return Util.Beans.Objects.Object is
   begin
      return From.Row;
   end Get_Row;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (Integer (From.List.Length));
      elsif Name = "rowIndex" then
         return Util.Beans.Objects.To_Object (From.Current_Index);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Deletes the list bean
   --  ------------------------------
   procedure Free (List : in out Util.Beans.Basic.Readonly_Bean_Access) is

      procedure Free is
        new Ada.Unchecked_Deallocation (List_Bean'Class,
                                        List_Bean_Access);
   begin
      if List.all in List_Bean'Class then
         declare
            L : List_Bean_Access := List_Bean (List.all)'Unchecked_Access;
         begin
            Free (L);
            List := null;
         end;
      end if;
   end Free;

end Util.Beans.Basic.Lists;
