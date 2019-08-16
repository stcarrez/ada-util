-----------------------------------------------------------------------
--  util-refs -- Reference Counting
--  Copyright (C) 2010, 2011, 2015, 2019 Stephane Carrez
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
with Util.Concurrent.Counters;

--  The <b>Util.Refs</b> package provides support to implement object reference counting.
--
--  The data type to share through reference counting has to inherit from <b>Ref_Entity</b>
--  and the generic package <b>References</b> has to be instantiated.
--  <pre>
--    type Data is new Util.Refs.Ref_Entity with record ... end record;
--    type Data_Access is access all Data;
--
--    package Data_Ref is new Utils.Refs.References (Data, Data_Access);
--  </pre>
--
--  The reference is used as follows:
--
--  <pre>
--     D  : Data_Ref.Ref := Data_Ref.Create;  --  Allocate and get a reference
--     D2 : Data_Ref.Ref := D;                --  Share reference
--     D.Value.XXXX  := 0;                    --  Set data member XXXX
--  </pre>
--
--  When a reference is shared in a multi-threaded environment, the reference has to
--  be protected by using the <b>References.Atomic_Ref</b> type.
--
--     R : Data_Ref.Atomic_Ref;
--
--  The reference is then obtained by the protected operation <b>Get</b>.
--
--     D : Data_Ref.Ref := R.Get;
--
package Util.Refs is

   pragma Preelaborate;

   --  Root of referenced objects.
   type Ref_Entity is abstract tagged limited private;

   --  Finalize the referenced object.  This is called before the object is freed.
   procedure Finalize (Object : in out Ref_Entity) is null;

   generic
      type Element_Type (<>) is new Ref_Entity with private;
      type Element_Access is access all Element_Type;
   package Indefinite_References is

      type Element_Accessor (Element : not null access Element_Type) is limited private
        with Implicit_Dereference => Element;

      type Ref is new Ada.Finalization.Controlled with private;

      --  Create an element and return a reference to that element.
      function Create (Value : in Element_Access) return Ref;

      function Value (Object : in Ref'Class) return Element_Accessor with Inline;

      --  Returns true if the reference does not contain any element.
      function Is_Null (Object : in Ref'Class) return Boolean with Inline_Always;

      function "=" (Left, Right : in Ref'Class) return Boolean with Inline_Always;

      --  The <b>Atomic_Ref</b> protected type defines a reference to an
      --  element which can be obtained and changed atomically.  The default
      --  Ada construct:
      --
      --     Ref1 := Ref2;
      --
      --  does not guarantee atomicity of the copy (assignment) and the increment
      --  of the reference counter (Adjust operation).  To replace shared reference
      --  by another one, the whole assignment and Adjust have to be protected.
      --  This is achieved by this protected type through the <b>Get</b> and <b>Set</b>
      generic
      package Atomic is
         protected type Atomic_Ref is
            --  Get the reference
            function Get return Ref;

            --  Change the reference
            procedure Set (Object : in Ref);
         private
            Value : Ref;
         end Atomic_Ref;
      end Atomic;

   private

      type Element_Accessor (Element : not null access Element_Type) is null record;

      type Ref is new Ada.Finalization.Controlled with record
         Target : Element_Access := null;
      end record;

      --  Release the reference.  Invoke <b>Finalize</b> and free the storage if it was
      --  the last reference.
      overriding
      procedure Finalize (Obj : in out Ref);

      --  Update the reference counter after an assignment.
      overriding
      procedure Adjust (Obj : in out Ref);

   end Indefinite_References;

   generic
      type Element_Type is new Ref_Entity with private;
      type Element_Access is access all Element_Type;
   package References is
      package IR is new Indefinite_References (Element_Type, Element_Access);

      subtype Ref is IR.Ref;
      subtype Element_Accessor is IR.Element_Accessor;

      --  Create an element and return a reference to that element.
      function Create return Ref;

      --  The <b>Atomic_Ref</b> protected type defines a reference to an
      --  element which can be obtained and changed atomically.  The default
      --  Ada construct:
      --
      --     Ref1 := Ref2;
      --
      --  does not guarantee atomicity of the copy (assignment) and the increment
      --  of the reference counter (Adjust operation).  To replace shared reference
      --  by another one, the whole assignment and Adjust have to be protected.
      --  This is achieved by this protected type through the <b>Get</b> and <b>Set</b>

   end References;

   generic
      type Element_Type is limited private;
      with procedure Finalize (Object : in out Element_Type) is null;
   package General_References is

      type Element_Accessor (Element : not null access Element_Type) is limited private
        with Implicit_Dereference => Element;

      type Ref is new Ada.Finalization.Controlled with private;

      --  Create an element and return a reference to that element.
      function Create return Ref;

      function Value (Object : in Ref'Class) return Element_Accessor with Inline;

      --  Returns true if the reference does not contain any element.
      function Is_Null (Object : in Ref'Class) return Boolean with Inline_Always;

      function "=" (Left, Right : in Ref'Class) return Boolean with Inline_Always;

      --  The <b>Atomic_Ref</b> protected type defines a reference to an
      --  element which can be obtained and changed atomically.  The default
      --  Ada construct:
      --
      --     Ref1 := Ref2;
      --
      --  does not guarantee atomicity of the copy (assignment) and the increment
      --  of the reference counter (Adjust operation).  To replace shared reference
      --  by another one, the whole assignment and Adjust have to be protected.
      --  This is achieved by this protected type through the <b>Get</b> and <b>Set</b>
      generic
      package Atomic is
         protected type Atomic_Ref is
            --  Get the reference
            function Get return Ref;

            --  Change the reference
            procedure Set (Object : in Ref);
         private
            Value : Ref;
         end Atomic_Ref;
      end Atomic;

   private

      type Element_Accessor (Element : not null access Element_Type) is null record;

      type Ref_Data is limited record
         Ref_Counter : Util.Concurrent.Counters.Counter;
         Data        : aliased Element_Type;
      end record;
      type Ref_Data_Access is access all Ref_Data;

      type Ref is new Ada.Finalization.Controlled with record
         Target : Ref_Data_Access := null;
      end record;

      --  Release the reference.  Invoke <b>Finalize</b> and free the storage if it was
      --  the last reference.
      overriding
      procedure Finalize (Obj : in out Ref);

      --  Update the reference counter after an assignment.
      overriding
      procedure Adjust (Obj : in out Ref);

   end General_References;

private

   type Ref_Entity is abstract tagged limited record
      Ref_Counter : Util.Concurrent.Counters.Counter;
   end record;

end Util.Refs;
