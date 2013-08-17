-----------------------------------------------------------------------
--  Util.Beans.Objects.Lists -- List bean holding some object
--  Copyright (C) 2013 Stephane Carrez
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

package body Util.Beans.Objects.Lists is

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   function Get_Count (From : in List_Bean) return Natural is
   begin
      return Natural (Vectors.Length (From.List));
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   procedure Set_Row_Index (From  : in out List_Bean;
                            Index : in Natural) is
   begin
      From.Current := Index;
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   function Get_Row (From : in List_Bean) return Util.Beans.Objects.Object is
   begin
      return Vectors.Element (From.List, From.Current - 1);
   end Get_Row;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : in List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (Integer (From.List.Length));
      elsif Name = "rowIndex" then
         return Util.Beans.Objects.To_Object (From.Current);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

end Util.Beans.Objects.Lists;
