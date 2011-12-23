-----------------------------------------------------------------------
--  util-beans-basic-ranges -- Range of values with helper for list iteration
--  Copyright (C) 2011 Stephane Carrez
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

package body Util.Beans.Basic.Ranges is

   --  ------------------------------
   --  Create a range definition.
   --  ------------------------------
   function Create (First, Last : in T) return Range_Bean is
      Result : Range_Bean;
   begin
      Result.First   := First;
      Result.Last    := Last;
      Result.Current := First;
      return Result;
   end Create;

   --  ------------------------------
   --  Get the range lower bound.
   --  ------------------------------
   function Get_First (From : in Range_Bean) return T is
   begin
      return From.First;
   end Get_First;

   --  ------------------------------
   --  Get the range upper bound.
   --  ------------------------------
   function Get_Last (From : in Range_Bean) return T is
   begin
      return From.Last;
   end Get_Last;

   --  ------------------------------
   --  Get the current value within the first/last bounds.
   --  ------------------------------
   function Get_Current (From : in Range_Bean) return T is
   begin
      return From.Current;
   end Get_Current;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Range_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "first" then
         return To_Object (From.First);
      elsif Name = "last" then
         return To_Object (From.Last);
      elsif Name = "value" then
         return To_Object (From.Current);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : in Range_Bean) return Natural is
   begin
      if From.Last < From.First then
         return 0;
      else
         return Natural (T'Pos (From.Last) - T'Pos (From.First)) + 1;
      end if;
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out Range_Bean;
                            Index : in Natural) is
   begin
      From.Current := T'Val (T'Pos (From.First) + Index - 1);
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : in Range_Bean) return Util.Beans.Objects.Object is
   begin
      return To_Object (From.Current);
   end Get_Row;

end Util.Beans.Basic.Ranges;
