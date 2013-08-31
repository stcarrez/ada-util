-----------------------------------------------------------------------
--  Util.Beans.Objects.Pairs -- Pairs of objects
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

package Util.Beans.Objects.Pairs is

   --  The <tt>Pair</tt> type is a bean that contains two values named first/key and
   --  second/value.
   type Pair is new Util.Beans.Basic.Bean with record
      First  : Object;
      Second : Object;
   end record;
   type Pair_Access is access all Pair'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Pair;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Pair;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Return an object represented by the pair of two values.
   function To_Object (First, Second : in Object) return Object;

end Util.Beans.Objects.Pairs;
