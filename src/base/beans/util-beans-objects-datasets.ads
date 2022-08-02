-----------------------------------------------------------------------
--  util-beans-objects-datasets -- Datasets
--  Copyright (C) 2013, 2018, 2022 Stephane Carrez
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
with Ada.Strings.Hash;
with Ada.Finalization;
with Ada.Containers.Indefinite_Hashed_Maps;

with Util.Beans.Basic;
with Util.Beans.Objects.Iterators;

--  == Datasets ==
--  The `Datasets` package implements the `Dataset` list bean which
--  defines a set of objects organized in rows and columns.  The `Dataset`
--  implements the `List_Bean` interface and allows to iterate over its rows.
--  Each row defines a `Bean` instance and allows to access each column value.
--  Each column is associated with a unique name.  The row `Bean` allows to
--  get or set the column by using the column name.
--
--     with Util.Beans.Objects.Datasets;
--     ...
--        Set : Util.Beans.Objects.Datasets.Dataset_Access
--            := new Util.Beans.Objects.Datasets.Dataset;
--
--  After creation of the dataset instance, the first step is to define
--  the columns that composed the list.  This is done by using the `Add_Column`
--  procedure:
--
--     Set.Add_Column ("name");
--     Set.Add_Column ("email");
--     Set.Add_Column ("age");
--
--  To populate the dataset, the package only provide the `Append` procedure
--  which adds a new row and calls a procedure whose job is to fill the columns
--  of the new row.  The procedure gets the row as an array of `Object`:
--
--     procedure Fill (Row : in out Util.Beans.Objects.Object_Array) is
--     begin
--        Row (Row'First) := To_Object (String '("Yoda"));
--        Row (Row'First + 1) := To_Object (String '("Yoda@Dagobah"));
--        Row (Row'First + 2) := To_Object (Integer (900));
--     end Fill;
--
--     Set.Append (Fill'Access);
--
--  The dataset instance is converted to an `Object` by using the `To_Object`
--  function.  Note that the default behavior of `To_Object` is to take
--  the ownership of the object and hence it will be released automatically.
--
--     List : Util.Beans.Objects.Object
--        := Util.Beans.Objects.To_Object (Set);
--
package Util.Beans.Objects.Datasets is

   subtype Iterator_Bean is Util.Beans.Objects.Iterators.Iterator_Bean;
   subtype Object_Array is Util.Beans.Objects.Object_Array;

   Invalid_State : exception;

   type Dataset is new Util.Beans.Basic.List_Bean and Iterator_Bean with private;
   type Dataset_Access is access all Dataset'Class;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : in Dataset) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out Dataset;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in Dataset) return Util.Beans.Objects.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Dataset;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Get an iterator to iterate starting with the first element.
   overriding
   function First (From : in Dataset) return Iterators.Proxy_Iterator_Access;

   --  Get an iterator to iterate starting with the last element.
   overriding
   function Last (From : in Dataset) return Iterators.Proxy_Iterator_Access;

   --  Append a row in the dataset and call the fill procedure to populate
   --  the row content.
   procedure Append (Into : in out Dataset;
                     Fill : not null access procedure (Data : in out Object_Array));

   --  Add a column to the dataset.  If the position is not specified,
   --  the column count is incremented and the name associated with the last column.
   --  Raises Invalid_State exception if the dataset contains some rows,
   procedure Add_Column (Into : in out Dataset;
                         Name : in String;
                         Pos  : in Natural := 0);

   --  Clear the content of the dataset.
   procedure Clear (Set : in out Dataset);

private

   type Object_Array_Access is access all Object_Array;

   type Dataset_Array is array (Positive range <>) of Object_Array_Access;
   type Dataset_Array_Access is access all Dataset_Array;

   package Dataset_Map is
      new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                 Element_Type    => Positive,
                                                 Hash            => Ada.Strings.Hash,
                                                 Equivalent_Keys => "=",
                                                 "="             => "=");

   type Row is new Util.Beans.Basic.Bean and Iterator_Bean with record
      Data : Object_Array_Access;
      Map  : access Dataset_Map.Map;
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Row;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Row;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Get an iterator to iterate starting with the first element.
   overriding
   function First (From : in Row) return Iterators.Proxy_Iterator_Access;

   --  Get an iterator to iterate starting with the last element.
   overriding
   function Last (From : in Row) return Iterators.Proxy_Iterator_Access;

   type Dataset is new Ada.Finalization.Controlled
     and Util.Beans.Basic.List_Bean and Iterator_Bean with record
      Data        : Dataset_Array_Access;
      Count       : Natural := 0;
      Columns     : Natural := 0;
      Map         : aliased Dataset_Map.Map;
      Current     : aliased Row;
      Current_Pos : Natural := 0;
      Row         : Util.Beans.Objects.Object;
   end record;

   --  Initialize the dataset and the row bean instance.
   overriding
   procedure Initialize (Set : in out Dataset);

   --  Release the dataset storage.
   overriding
   procedure Finalize (Set : in out Dataset);

   type Dataset_Iterator is new Iterators.Proxy_Iterator with record
      Current     : aliased Row;
      Current_Pos : Natural := 0;
      Row         : Util.Beans.Objects.Object;
   end record;
   type Dataset_Iterator_Access is access all Dataset_Iterator;

   overriding
   procedure Initialize (Iter : in out Dataset_Iterator);

   overriding
   function Has_Element (Iter : in Dataset_Iterator) return Boolean;

   overriding
   procedure Next (Iter : in out Dataset_Iterator);

   overriding
   procedure Previous (Iter : in out Dataset_Iterator);

   overriding
   function Element (Iter : in Dataset_Iterator) return Object;

   type Row_Iterator is new Iterators.Proxy_Map_Iterator with record
      Pos  : Dataset_Map.Cursor;
      Data : Object_Array_Access;
   end record;
   type Row_Iterator_Access is access all Row_Iterator;

   overriding
   procedure Initialize (Iter : in out Row_Iterator);

   overriding
   function Has_Element (Iter : in Row_Iterator) return Boolean;

   overriding
   procedure Next (Iter : in out Row_Iterator);

   overriding
   procedure Previous (Iter : in out Row_Iterator);

   overriding
   function Element (Iter : in Row_Iterator) return Object;

   overriding
   function Key (Iter : in Row_Iterator) return String;

end Util.Beans.Objects.Datasets;
