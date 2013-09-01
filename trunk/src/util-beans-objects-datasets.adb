-----------------------------------------------------------------------
--  Util.Beans.Objects.Datasets -- Datasets
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
with Ada.Unchecked_Deallocation;

package body Util.Beans.Objects.Datasets is

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => Object_Array,
                                      Name   => Object_Array_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Dataset_Array,
                                     Name   => Dataset_Array_Access);

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : in Dataset) return Natural is
   begin
      return From.Count;
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out Dataset;
                            Index : in Natural) is
   begin
      From.Current_Pos  := Index;
      From.Current.Data := From.Data (Index);
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : in Dataset) return Util.Beans.Objects.Object is
   begin
      return From.Row;
   end Get_Row;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Dataset;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (From.Count);
      elsif Name = "rowIndex" then
         return Util.Beans.Objects.To_Object (From.Current_Pos);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Append a row in the dataset and call the fill procedure to populate
   --  the row content.
   --  ------------------------------
   procedure Append (Into : in out Dataset;
                     Fill : not null access procedure (Data : in out Object_Array)) is
      Data : constant Object_Array_Access := new Object_Array (1 .. Into.Columns);
   begin
      if Into.Data = null then
         Into.Data := new Dataset_Array (1 .. 10);
      elsif Into.Count >= Into.Data'Length then
         declare
            --  Sun's Java ArrayList use a 2/3 grow factor.
            --  Python's array use 8/9.
            Grow : constant Positive := Into.Count + (Into.Count * 2) / 3;
            Set  : constant Dataset_Array_Access := new Dataset_Array (1 .. Grow);
         begin
            Set (Into.Data'Range) := Into.Data.all;
            Free (Into.Data);
            Into.Data := Set;
         end;
      end if;
      Into.Count := Into.Count + 1;
      Into.Data (Into.Count) := Data;
      Fill (Data.all);
   end Append;

   --  ------------------------------
   --  Add a column to the dataset.  If the position is not specified,
   --  the column count is incremented and the name associated with the last column.
   --  Raises Invalid_State exception if the dataset contains some rows,
   --  ------------------------------
   procedure Add_Column (Into : in out Dataset;
                         Name : in String;
                         Pos  : in Natural := 0) is
      Col : Positive;
   begin
      if Into.Count /= 0 then
         raise Invalid_State with "The dataset contains some rows.";
      end if;
      if Pos = 0 then
         Col := Into.Columns + 1;
      else
         Col := Pos;
      end if;
      Into.Map.Insert (Name, Col);
      if Into.Columns < Col then
         Into.Columns := Col;
      end if;
   end Add_Column;

   --  ------------------------------
   --  Clear the content of the dataset.
   --  ------------------------------
   procedure Clear (Set : in out Dataset) is
   begin
      for I in 1 .. Set.Count loop
         Free (Set.Data (I));
      end loop;
      Set.Count        := 0;
      Set.Current_Pos  := 0;
      Set.Current.Data := null;
   end Clear;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Row;
                       Name : in String) return Util.Beans.Objects.Object is
      Pos : constant Dataset_Map.Cursor := From.Map.Find (Name);
   begin
      if From.Data /= null and then Dataset_Map.Has_Element (Pos) then
         return From.Data (Dataset_Map.Element (Pos));
      else
         return Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Row;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
      Pos : constant Dataset_Map.Cursor := From.Map.Find (Name);
   begin
      if From.Data /= null and then Dataset_Map.Has_Element (Pos) then
         From.Data (Dataset_Map.Element (Pos)) := Value;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Initialize the dataset and the row bean instance.
   --  ------------------------------
   overriding
   procedure Initialize (Set : in out Dataset) is
   begin
      Set.Row := To_Object (Value   => Set.Current'Unchecked_Access,
                            Storage => STATIC);
      Set.Current.Map := Set.Map'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   --  Release the dataset storage.
   --  ------------------------------
   overriding
   procedure Finalize (Set : in out Dataset) is
   begin
      Set.Clear;
      Free (Set.Data);
   end Finalize;

end Util.Beans.Objects.Datasets;
