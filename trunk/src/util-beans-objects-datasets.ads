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
with Ada.Strings.Hash;
with Ada.Finalization;
with Ada.Containers.Indefinite_Hashed_Maps;

with Util.Beans.Basic;

--  == Introduction ==
--  The <tt>Datasets</tt> package implements the <tt>Dataset</tt> list bean which
--  defines a set of objects organized in rows and columns.  The <tt>Dataset</tt>
--  implements the <tt>List_Bean</tt> interface and allows to iterate over its rows.
--  Each row defines a <tt>Bean</tt> instance and allows to access each column value.
--  Each column is associated with a unique name.  The row <tt>Bean</tt> allows to
--  get or set the column by using the column name.
package Util.Beans.Objects.Datasets is

   Invalid_State : exception;

   --  An array of objects.
   type Object_Array is array (Positive range <>) of Object;

   type Dataset is new Util.Beans.Basic.List_Bean with private;

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

   type Row is new Util.Beans.Basic.Bean with record
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

   type Dataset is new Ada.Finalization.Controlled and Util.Beans.Basic.List_Bean with record
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

end Util.Beans.Objects.Datasets;
