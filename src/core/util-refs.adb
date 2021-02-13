-----------------------------------------------------------------------
--  util-refs -- Reference Counting
--  Copyright (C) 2010, 2011, 2019, 2020 Stephane Carrez
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

with Ada.Unchecked_Deallocation;
package body Util.Refs is

   package body Indefinite_References is

      --  ------------------------------
      --  Create an element and return a reference to that element.
      --  ------------------------------
      function Create (Value : in Element_Access) return Ref is
      begin
         return Result : Ref do
            Result.Target := Value;
            Util.Concurrent.Counters.Increment (Result.Target.Ref_Counter);
         end return;
      end Create;

      --  ------------------------------
      --  Get the element access value.
      --  ------------------------------
      function Value (Object : in Ref'Class) return Element_Accessor is
      begin
         return Element_Accessor '(Element => Object.Target);
      end Value;

      --  ------------------------------
      --  Returns true if the reference does not contain any element.
      --  ------------------------------
      function Is_Null (Object : in Ref'Class) return Boolean is
      begin
         return Object.Target = null;
      end Is_Null;

      function "=" (Left, Right : in Ref'Class) return Boolean is
      begin
         return Left.Target = Right.Target;
      end "=";

      package body Atomic is
         protected body Atomic_Ref is
            --  ------------------------------
            --  Get the reference
            --  ------------------------------
            function Get return Ref is
            begin
               return Value;
            end Get;

            --  ------------------------------
            --  Change the reference
            --  ------------------------------
            procedure Set (Object : in Ref) is
            begin
               Value := Object;
            end Set;

         end Atomic_Ref;
      end Atomic;

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Element_Type,
                                        Name   => Element_Access);

      --  ------------------------------
      --  Release the reference.  Invoke <b>Finalize</b> and free the storage if it was
      --  the last reference.
      --  ------------------------------
      overriding
      procedure Finalize (Obj : in out Ref) is
         Release : Boolean;
      begin
         if Obj.Target /= null then
            Util.Concurrent.Counters.Decrement (Obj.Target.Ref_Counter, Release);
            if Release then
               Obj.Target.Finalize;
               Free (Obj.Target);
            else
               Obj.Target := null;
            end if;
         end if;
      end Finalize;

      --  ------------------------------
      --  Update the reference counter after an assignment.
      --  ------------------------------
      overriding
      procedure Adjust (Obj : in out Ref) is
      begin
         if Obj.Target /= null then
            Util.Concurrent.Counters.Increment (Obj.Target.Ref_Counter);
         end if;
      end Adjust;

   end Indefinite_References;

   package body References is

      --  ------------------------------
      --  Create an element and return a reference to that element.
      --  ------------------------------
      function Create return Ref is
      begin
         return IR.Create (new Element_Type);
      end Create;
   end References;

   package body General_References is

      --  ------------------------------
      --  Create an element and return a reference to that element.
      --  ------------------------------
      function Create return Ref is
      begin
         return Result : Ref do
            Result.Target := new Ref_Data;
            Util.Concurrent.Counters.Increment (Result.Target.Ref_Counter);
         end return;
      end Create;

      --  ------------------------------
      --  Get the element access value.
      --  ------------------------------
      function Value (Object : in Ref'Class) return Element_Accessor is
      begin
         --  GCC 10 requires the Unrestricted_Access while GCC < 10 allowed Access...
         --  It is safe because the Ref handles the copy and Element_Accessor is limited.
         return Element_Accessor '(Element => Object.Target.Data'Unrestricted_Access);
      end Value;

      --  ------------------------------
      --  Returns true if the reference does not contain any element.
      --  ------------------------------
      function Is_Null (Object : in Ref'Class) return Boolean is
      begin
         return Object.Target = null;
      end Is_Null;

      function "=" (Left, Right : in Ref'Class) return Boolean is
      begin
         return Left.Target = Right.Target;
      end "=";

      package body Atomic is
         protected body Atomic_Ref is
            --  ------------------------------
            --  Get the reference
            --  ------------------------------
            function Get return Ref is
            begin
               return Value;
            end Get;

            --  ------------------------------
            --  Change the reference
            --  ------------------------------
            procedure Set (Object : in Ref) is
            begin
               Value := Object;
            end Set;

         end Atomic_Ref;
      end Atomic;

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Ref_Data,
                                        Name   => Ref_Data_Access);

      --  ------------------------------
      --  Release the reference.  Invoke <b>Finalize</b> and free the storage if it was
      --  the last reference.
      --  ------------------------------
      overriding
      procedure Finalize (Obj : in out Ref) is
         Release : Boolean;
      begin
         if Obj.Target /= null then
            Util.Concurrent.Counters.Decrement (Obj.Target.Ref_Counter, Release);
            if Release then
               Finalize (Obj.Target.Data);
               Free (Obj.Target);
            else
               Obj.Target := null;
            end if;
         end if;
      end Finalize;

      --  ------------------------------
      --  Update the reference counter after an assignment.
      --  ------------------------------
      overriding
      procedure Adjust (Obj : in out Ref) is
      begin
         if Obj.Target /= null then
            Util.Concurrent.Counters.Increment (Obj.Target.Ref_Counter);
         end if;
      end Adjust;

   end General_References;

end Util.Refs;
