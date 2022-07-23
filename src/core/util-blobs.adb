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
package body Util.Blobs is

   use Ada.Streams;

   --  ------------------------------
   --  Create a blob with an allocated buffer of <b>Size</b> bytes.
   --  ------------------------------
   function Create_Blob (Size : in Natural) return Blob_Ref is
      B :  constant Blob_Access := new Blob '(Refs.Ref_Entity with
                                              Len    => Stream_Element_Offset (Size),
                                              others => <>);
   begin
      return Blob_References.Create (B);
   end Create_Blob;

   --  ------------------------------
   --  Create a blob initialized with the given data buffer.
   --  ------------------------------
   function Create_Blob (Data : in Ada.Streams.Stream_Element_Array) return Blob_Ref is
      B   :  constant Blob_Access := new Blob '(Refs.Ref_Entity with
                                                Len    => Data'Length,
                                                Data   => Data);
   begin
      return Blob_References.Create (B);
   end Create_Blob;

   --  ------------------------------
   --  Create a blob initialized with the given data buffer.
   --  ------------------------------
   function Create_Blob (Content : in String) return Blob_Ref is
      Data : Ada.Streams.Stream_Element_Array (1 .. Content'Length);
      for Data'Address use Content'Address;
   begin
      return Create_Blob (Data);
   end Create_Blob;

   --  ------------------------------
   --  Return a null blob.
   --  ------------------------------
   function Null_Blob return Blob_Ref is
      Null_Blob_Instance : Blob_Ref;
   begin
      return Null_Blob_Instance;
   end Null_Blob;

end Util.Blobs;
