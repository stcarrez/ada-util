-----------------------------------------------------------------------
--  util-beans-factory -- Bean Registration and Factory
--  Copyright (C) 2010, 2018, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Methods;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

--  The <b>EL.Beans.Factory</b> package is a registry for the creation
--  of request, session and application beans.
package Util.Beans.Factory is

   --  The bean definition is a small descriptor used by the
   --  factory to create the bean instance when it is needed.
   --  It also defines the methods that can be specified and
   --  invoked as part of a <b>Method_Expression</b>.
   type Bean_Definition (Method_Count : Natural) is abstract tagged limited record
      Methods : Util.Beans.Methods.Method_Binding_Array (1 .. Method_Count);
   end record;
   type Bean_Definition_Access is access constant Bean_Definition'Class;

   --  Create a bean.
   function Create (Def : in Bean_Definition)
                    return Util.Beans.Basic.Readonly_Bean_Access is abstract;

   --  Free the bean instance.
   procedure Destroy (Def  : in Bean_Definition;
                      Bean : in out Util.Beans.Basic.Readonly_Bean_Access) is abstract;

   --  Defines the scope of the bean instance.
   type Scope_Type is
     (
      --  Application scope means the bean is shared by all sessions and requests
      APPLICATION_SCOPE,

      --  Session scope means the bean is created one for each session.
      SESSION_SCOPE,

      --  Request scope means the bean is created for each request
      REQUEST_SCOPE,

      ANY_SCOPE);

   --  ------------------------------
   --  Binding
   --  ------------------------------
   type Binding is interface;
   type Binding_Access is access all Binding'Class;

   procedure Create (Factory    : in Binding;
                     Name       : in Ada.Strings.Unbounded.Unbounded_String;
                     Result     : out Util.Beans.Basic.Readonly_Bean_Access;
                     Definition : out Bean_Definition_Access;
                     Scope      : out Scope_Type) is abstract;

   --  ------------------------------
   --  Bean Factory
   --  ------------------------------
   --  Factory for bean creation
   type Bean_Factory is limited private;

   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   procedure Register (Factory    : in out Bean_Factory;
                       Name       : in String;
                       Definition : in Bean_Definition_Access;
                       Scope      : in Scope_Type := REQUEST_SCOPE);

   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   procedure Register (Factory : in out Bean_Factory;
                       Name    : in String;
                       Bind    : in Binding_Access);

   --  Register all the definitions from a factory to a main factory.
   procedure Register (Factory : in out Bean_Factory;
                       From    : in Bean_Factory);

   --  Create a bean by using the create operation registered for the name
   procedure Create (Factory    : in Bean_Factory;
                     Name       : in Ada.Strings.Unbounded.Unbounded_String;
                     Result     : out Util.Beans.Basic.Readonly_Bean_Access;
                     Definition : out Bean_Definition_Access;
                     Scope      : out Scope_Type);

   type Simple_Binding is new Binding with private;

   overriding
   procedure Create (Factory    : in Simple_Binding;
                     Name       : in Ada.Strings.Unbounded.Unbounded_String;
                     Result     : out Util.Beans.Basic.Readonly_Bean_Access;
                     Definition : out Bean_Definition_Access;
                     Scope      : out Scope_Type);

private

   type Simple_Binding is new Binding with record
      Def    : Bean_Definition_Access;
      Scope  : Scope_Type;
   end record;
   type Simple_Binding_Access is access all Simple_Binding;

   use Ada.Strings.Unbounded;

   package Bean_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type     => Unbounded_String,
                                            Element_Type => Binding_Access,
                                            Hash         => Ada.Strings.Unbounded.Hash,
                                            Equivalent_Keys => "=");

   type Bean_Factory is limited record
      Map : Bean_Maps.Map;
   end record;

end Util.Beans.Factory;
