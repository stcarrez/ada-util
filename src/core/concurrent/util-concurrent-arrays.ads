-----------------------------------------------------------------------
--  util-concurrent-arrays -- Concurrent Arrays
--  Copyright (C) 2012, 2017, 2022 Stephane Carrez
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

--  == Introduction ==
--  The <b>Util.Concurrent.Arrays</b> generic package defines an array which provides a
--  concurrent read only access and a protected exclusive write access.  This implementation
--  is intended to be used in applications that have to frequently iterate over the array
--  content.  Adding or removing elements in the array is assumed to be a not so frequent
--  operation.  Based on these assumptions, updating the array is implemented by
--  using the <tt>copy on write</tt> design pattern.  Read access is provided through a
--  reference object that can be shared by multiple readers.
--
--  == Declaration ==
--  The package must be instantiated using the element type representing the array element.
--
--    package My_Array is new Util.Concurrent.Arrays (Element_Type => Integer);
--
--  == Adding Elements ==
--  The vector instance is declared and elements are added as follows:
--
--     C : My_Array.Vector;
--
--     C.Append (E1);
--
--  == Iterating over the array ==
--  To read and iterate over the vector, a task will get a reference to the vector array
--  and it will iterate over it.  The reference will be held until the reference object is
--  finalized.  While doing so, if another task updates the vector, a new vector array will
--  be associated with the vector instance (but this will not change the reader's references).
--
--     R : My_Array.Ref := C.Get;
--
--     R.Iterate (Process'Access);
--     ...
--     R.Iterate (Process'Access);
--
--  In the above example, the two `Iterate` operations will iterate over the same list of
--  elements, even if another task appends an element in the middle.
--
--  Notes:
--    * This package is close to the Java class `java.util.concurrent.CopyOnWriteArrayList`.
--
--    * The package implements voluntarily a very small subset of `Ada.Containers.Vectors`.
--
--    * The implementation does not use the Ada container for performance and size reasons.
--
--    * The `Iterate` and `Reverse_Iterate` operation give a direct access to the element.
generic
   type Element_Type is private;

   with function "=" (Left, Right : in Element_Type) return Boolean is <>;
package Util.Concurrent.Arrays is

   --  The reference to the read-only vector elements.
   type Ref is tagged private;

   --  Returns True if the container is empty.
   function Is_Empty (Container : in Ref) return Boolean;

   --  Iterate over the vector elements and execute the <b>Process</b> procedure
   --  with the element as parameter.
   procedure Iterate (Container : in Ref;
                      Process   : not null access procedure (Item : in Element_Type));

   --  Iterate over the vector elements in reverse order and execute the <b>Process</b> procedure
   --  with the element as parameter.
   procedure Reverse_Iterate (Container : in Ref;
                              Process   : not null access procedure (Item : in Element_Type));

   --  Vector of elements.
   type Vector is new Ada.Finalization.Limited_Controlled with private;

   --  Get a read-only reference to the vector elements.  The referenced vector will never
   --  be modified.
   function Get (Container : in Vector'Class) return Ref;

   --  Append the element to the vector.  The modification will not be visible to readers
   --  until they call the <b>Get</b> function.
   procedure Append (Container : in out Vector;
                     Item      : in Element_Type);

   --  Remove the element represented by <b>Item</b> from the vector.  The modification will
   --  not be visible to readers until they call the <b>Get</b> function.
   procedure Remove (Container : in out Vector;
                     Item      : in Element_Type);

   --  Release the vector elements.
   overriding
   procedure Finalize (Object : in out Vector);

private

   --  To store the vector elements, we use an array which is allocated dynamically.
   --  The generated code is smaller compared to the use of Ada vectors container.
   type Element_Array is array (Positive range <>) of Element_Type;
   type Element_Array_Access is access all Element_Array;

   Null_Element_Array : constant Element_Array_Access := null;

   type Vector_Record (Len : Positive) is record
      Ref_Counter : Util.Concurrent.Counters.Counter;
      List        : Element_Array (1 .. Len);
   end record;
   type Vector_Record_Access is access all Vector_Record;

   type Ref is new Ada.Finalization.Controlled with record
      Target : Vector_Record_Access := null;
   end record;

   --  Release the reference.  Invoke <b>Finalize</b> and free the storage if it was
   --  the last reference.
   overriding
   procedure Finalize (Obj : in out Ref);

   --  Update the reference counter after an assignment.
   overriding
   procedure Adjust (Obj : in out Ref);

   --  Vector of objects
   protected type Protected_Vector is

      --  Get a readonly reference to the vector.
      function Get return Ref;

      --  Append the element to the vector.
      procedure Append (Item : in Element_Type);

      --  Remove the element from the vector.
      procedure Remove (Item : in Element_Type);

   private
      Elements : Ref;
   end Protected_Vector;

   type Vector is new Ada.Finalization.Limited_Controlled with record
      List : Protected_Vector;
   end record;

end Util.Concurrent.Arrays;
