-----------------------------------------------------------------------
--  util-beans-lists -- Beans implementing the List interface
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

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Strings.Vectors;

package Util.Beans.Lists.Strings is

   --  The list of elements is defined in a public part so that applications
   --  can easily add or remove elements in the target list.  The <b>List_Bean</b>
   --  type holds the real implementation with the private parts.
   type Abstract_List_Bean is abstract new Util.Beans.Basic.List_Bean with record
      List : aliased Util.Strings.Vectors.Vector;
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

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in List_Bean) return Util.Beans.Objects.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

private

   type List_Bean is new Abstract_List_Bean with record
      Current : Natural := 0;
   end record;

end Util.Beans.Lists.Strings;
