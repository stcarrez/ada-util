-----------------------------------------------------------------------
--  util-http-parts -- HTTP Parts
--  Copyright (C) 2011, 2012 Stephane Carrez
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

--  The <b>ASF.Parts</b> package is an Ada implementation of the Java servlet part
--  (JSR 315 3. The Request) provided by the <tt>javax.servlet.http.Part</tt> class.
package Util.Http.Parts is

   --  ------------------------------
   --  Multi part content
   --  ------------------------------
   --  The <b>Part</b> type describes a mime part received in a request.
   --  The content is stored in a file and several operations are provided
   --  to manage the content.
   type Part is abstract new Ada.Finalization.Limited_Controlled with private;

   --  Get the size of the mime part.
   function Get_Size (Data : in Part) return Natural is abstract;

   --  Get the content name submitted in the mime part.
   function Get_Name (Data : in Part) return String is abstract;

   --  Get the path of the local file which contains the part.
   function Get_Local_Filename (Data : in Part) return String is abstract;

   --  Get the content type of the part.
   function Get_Content_Type (Data : in Part) return String is abstract;

   --  Write the part data item to the file.  This method is not guaranteed to succeed
   --  if called more than once for the same part. This allows a particular implementation
   --  to use, for example, file renaming, where possible, rather than copying all of
   --  the underlying data, thus gaining a significant performance benefit.
   procedure Save (Data : in Part;
                   Path : in String);

   --  Deletes the underlying storage for a file item, including deleting any associated
   --  temporary disk file.
   procedure Delete (Data : in out Part);

private

   type Part is abstract new Ada.Finalization.Limited_Controlled with null record;

end Util.Http.Parts;
