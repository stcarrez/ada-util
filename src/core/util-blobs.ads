-----------------------------------------------------------------------
--  util-blobs -- Binary data content with reference counting
--  Copyright (C) 2022 Stephane Carrez
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
with Ada.Streams;
with Util.Refs;
package Util.Blobs with Preelaborate is

   --  ------------------------------
   --  Blob data type
   --  ------------------------------
   --  The <b>Blob</b> type is used to represent database blobs.  The data is stored
   --  in an <b>Ada.Streams.Stream_Element_Array</b> pointed to by the <b>Data</b> member.
   --  The query statement and bind parameter will use a <b>Blob_Ref</b> which represents
   --  a reference to the blob data.  This is intended to minimize data copy.
   type Blob (Len : Ada.Streams.Stream_Element_Offset) is new Util.Refs.Ref_Entity with record
      Data : Ada.Streams.Stream_Element_Array (1 .. Len);
   end record;
   type Blob_Access is access all Blob;

   package Blob_References is new Util.Refs.Indefinite_References (Blob, Blob_Access);
   subtype Blob_Ref is Blob_References.Ref;
   subtype Blob_Accessor is Blob_References.Element_Accessor;

   --  Create a blob with an allocated buffer of <b>Size</b> bytes.
   function Create_Blob (Size : in Natural) return Blob_Ref;

   --  Create a blob initialized with the given data buffer.
   function Create_Blob (Data : in Ada.Streams.Stream_Element_Array) return Blob_Ref;

   --  Create a blob initialized with the given string.
   function Create_Blob (Content : in String) return Blob_Ref;

   --  Return a null blob.
   function Null_Blob return Blob_Ref;

end Util.Blobs;
