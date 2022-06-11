-----------------------------------------------------------------------
--  util-beans-basic-ranges -- Range of values with helper for list iteration
--  Copyright (C) 2011, 2022 Stephane Carrez
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

with Util.Beans.Objects;

--  The <b>Util.Beans.Basic.Ranges</b> generic package defines an object that holds
--  a range definition.  It implements the <b>List_Bean</b> interface which allows to
--  iterate over the values of the defined range.
generic
   type T is (<>);
   with function To_Object (From : in T) return Util.Beans.Objects.Object is <>;
package Util.Beans.Basic.Ranges is

   --  ------------------------------
   --  Range of discrete values
   --  ------------------------------
   --  The <b>Range_Bean</b> defines a discrete range.  It holds a lower and upper bound.
   --  A current value is also used for the <b>List_Bean</b> interface to iterate over the range.
   type Range_Bean is new Util.Beans.Basic.List_Bean with private;
   type Range_Bean_Access is access all Range_Bean'Class;

   --  Create a range definition.
   function Create (First, Last : in T) return Range_Bean;

   --  Get the range lower bound.
   function Get_First (From : in Range_Bean) return T;

   --  Get the range upper bound.
   function Get_Last (From : in Range_Bean) return T;

   --  Get the current value within the first/last bounds.
   function Get_Current (From : in Range_Bean) return T;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Range_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : in Range_Bean) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out Range_Bean;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in Range_Bean) return Util.Beans.Objects.Object;

private

   type Range_Bean is new Util.Beans.Basic.List_Bean with record
      First   : T := T'First;
      Last    : T := T'First;
      Current : T := T'First;
   end record;

end Util.Beans.Basic.Ranges;
