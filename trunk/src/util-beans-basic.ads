-----------------------------------------------------------------------
--  Util.Beans.Basic -- Interface Definition with Getter and Setters
--  Copyright (C) 2009, 2010 Stephane Carrez
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
package Util.Beans.Basic is

   pragma Preelaborate;

   --  ------------------------------
   --  Read-only Bean interface.
   --  ------------------------------
   --  The ''Readonly_Bean'' interface allows to plug a complex
   --  runtime object to the expression resolver.  This interface
   --  must be implemented by any tagged record that should be
   --  accessed as a variable for an expression.
   --
   --  For example, if 'foo' is bound to an object implementing that
   --  interface, expressions like 'foo.name' will resolve to 'foo'
   --  and the 'Get_Value' method will be called with 'name'.
   --
   type Readonly_Bean is limited interface;
   type Readonly_Bean_Access is access all Readonly_Bean'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : Readonly_Bean;
                       Name : String) return Util.Beans.Objects.Object is abstract;

   --  ------------------------------
   --  Bean interface.
   --  ------------------------------
   --  The ''Bean'' interface allows to modify a property value.
   type Bean is limited interface and Readonly_Bean;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   procedure Set_Value (From  : in out Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is abstract;

   --  ------------------------------
   --  List of objects
   --  ------------------------------
   --  The <b>List_Bean</b> interface gives access to a list of objects.
   type List_Bean is limited interface and Readonly_Bean;
   type List_Bean_Access is access all List_Bean'Class;

   --  Get the number of elements in the list.
   function Get_Count (From : List_Bean) return Natural is abstract;

   --  Set the current row index.  Valid row indexes start at 1.
   procedure Set_Row_Index (From  : in out List_Bean;
                            Index : in Natural) is abstract;

   --  Get the element at the current row index.
   function Get_Row (From  : List_Bean) return Util.Beans.Objects.Object is abstract;

end Util.Beans.Basic;
