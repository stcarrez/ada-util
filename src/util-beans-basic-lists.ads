-----------------------------------------------------------------------
--  util-beans-basic-lists -- List bean given access to a vector
--  Copyright (C) 2011, 2012, 2017 Stephane Carrez
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
with Ada.Containers;
with Ada.Containers.Vectors;
with Util.Beans.Objects;

--  The <b>Util.Beans.Basic.Lists</b> generic package implements a list of
--  elements that can be accessed through the <b>List_Bean</b> interface.
generic
   type Element_Type is new Util.Beans.Basic.Readonly_Bean with private;
package Util.Beans.Basic.Lists is

   --  Package that represents the vectors of elements.
   --  (gcc 4.4 crashes if this package is defined as generic parameter.
   package Vectors is
     new Ada.Containers.Vectors (Element_Type => Element_Type,
                                 Index_Type   => Positive);

   --  The list of elements is defined in a public part so that applications
   --  can easily add or remove elements in the target list.  The <b>List_Bean</b>
   --  type holds the real implementation with the private parts.
   type Abstract_List_Bean is abstract new Ada.Finalization.Controlled
     and Util.Beans.Basic.List_Bean with record
      List : aliased Vectors.Vector;
   end record;

   --  ------------------------------
   --  List of objects
   --  ------------------------------
   --  The <b>List_Bean</b> type gives access to a list of objects.
   type List_Bean is new Abstract_List_Bean with private;
   type List_Bean_Access is access all List_Bean'Class;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : in List_Bean) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out List_Bean;
                            Index : in Natural);

   --  Returns the current row index.
   function Get_Row_Index (From : in List_Bean) return Natural;

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in List_Bean) return Util.Beans.Objects.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Initialize the list bean.
   overriding
   procedure Initialize (Object : in out List_Bean);

   --  Deletes the list bean
   procedure Free (List : in out Util.Beans.Basic.Readonly_Bean_Access);

private

   type List_Bean is new Abstract_List_Bean with record
      Current       : aliased Element_Type;
      Current_Index : Natural := 0;
      Row           : Util.Beans.Objects.Object;
   end record;

end Util.Beans.Basic.Lists;
