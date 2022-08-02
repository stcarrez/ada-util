-----------------------------------------------------------------------
--  util-beans-factory -- Bean Registration and Factory
--  Copyright (C) 2009, 2010, 2015, 2018, 2022 Stephane Carrez
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

with Util.Log.Loggers;
package body Util.Beans.Factory is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Beans.Factory");

   --  ------------------------------
   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   --  ------------------------------
   procedure Register (Factory    : in out Bean_Factory;
                       Name       : in String;
                       Definition : in Bean_Definition_Access;
                       Scope      : in Scope_Type := REQUEST_SCOPE) is
      B : constant Simple_Binding_Access := new Simple_Binding '(Def   => Definition,
                                                                 Scope => Scope);
   begin
      Log.Info ("Register bean '{0}' in scope {1}", Name, Scope_Type'Image (Scope));

      Register (Factory, Name, B.all'Access);
   end Register;

   --  ------------------------------
   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   --  ------------------------------
   procedure Register (Factory : in out Bean_Factory;
                       Name    : in String;
                       Bind    : in Binding_Access) is
   begin
      Log.Info ("Register bean binding '{0}'", Name);

      Factory.Map.Include (Key => To_Unbounded_String (Name),
                           New_Item => Bind);
   end Register;

   --  ------------------------------
   --  Register all the definitions from a factory to a main factory.
   --  ------------------------------
   procedure Register (Factory : in out Bean_Factory;
                       From    : in Bean_Factory) is
      Pos : Bean_Maps.Cursor := Bean_Maps.First (From.Map);
   begin
      while Bean_Maps.Has_Element (Pos) loop
         Factory.Map.Include (Key      => Bean_Maps.Key (Pos),
                              New_Item => Bean_Maps.Element (Pos));
         Bean_Maps.Next (Pos);
      end loop;
   end Register;

   --  ------------------------------
   --  Create a bean by using the create operation registered for the name
   --  ------------------------------
   procedure Create (Factory    : in Bean_Factory;
                     Name       : in Unbounded_String;
                     Result     : out Util.Beans.Basic.Readonly_Bean_Access;
                     Definition : out Bean_Definition_Access;
                     Scope      : out Scope_Type) is
      Pos : constant Bean_Maps.Cursor := Factory.Map.Find (Name);
   begin
      if Bean_Maps.Has_Element (Pos) then
         declare
            B : constant Binding_Access := Bean_Maps.Element (Pos);
         begin
            B.Create (Name, Result, Definition, Scope);
         end;
      end if;
   end Create;

   overriding
   procedure Create (Factory    : in Simple_Binding;
                     Name       : in Ada.Strings.Unbounded.Unbounded_String;
                     Result     : out Util.Beans.Basic.Readonly_Bean_Access;
                     Definition : out Bean_Definition_Access;
                     Scope      : out Scope_Type) is
      pragma Unreferenced (Name);
   begin
      Result     := Factory.Def.Create;
      Definition := Factory.Def;
      Scope      := Factory.Scope;
   end Create;

end Util.Beans.Factory;
