-----------------------------------------------------------------------
--  util-texts-builders -- Text builder
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
with Ada.Finalization;

--  == Description ==
--  The <tt>Util.Texts.Builders</tt> generic package was designed to provide string builders.
--  The interface was designed to reduce memory copies as much as possible.
--
--    * The <tt>Builder</tt> type holds a list of chunks into which texts are appended.
--    * The builder type holds an initial chunk whose capacity is defined when the builder
--      instance is declared.
--    * There is only an <tt>Append</tt> procedure which allows to append text to the builder.
--      This is the only time when a copy is made.
--    * The package defines the <tt>Iterate</tt> operation that allows to get the content
--      collected by the builder.  When using the <tt>Iterate</tt> operation, no copy is
--      performed since chunks data are passed passed by reference.
--    * The type is limited to forbid copies of the builder instance.
--
--  == Example ==
--  First, instantiate the package for the element type (eg, String):
--
--    package String_Builder is new Util.Texts.Builders (Character, String);
--
--  Declare the string builder instance with its initial capacity:
--
--    Builder : String_Builder.Builder (256);
--
--  And append to it:
--
--    String_Builder.Append (Builder, "Hello");
--
--  To get the content collected in the builder instance, write a procedure that recieves
--  the chunk data as parameter:
--
--    procedure Collect (Item : in String) is ...
--
--  And use the <tt>Iterate</tt> operation:
--
--    String_Builder.Iterate (Builder, Collect'Access);
--
generic
   type Element_Type is (<>);
   type Input is array (Positive range <>) of Element_Type;
   Chunk_Size : Positive := 80;
package Util.Texts.Builders is

   pragma Preelaborate;

   type Builder (Len : Positive) is limited private;

   --  Get the length of the item builder.
   function Length (Source : in Builder) return Natural;

   --  Get the capacity of the builder.
   function Capacity (Source : in Builder) return Natural;

   --  Get the builder block size.
   function Block_Size (Source : in Builder) return Positive;

   --  Set the block size for the allocation of next chunks.
   procedure Set_Block_Size (Source : in out Builder;
                             Size   : in Positive);

   --  Append the <tt>New_Item</tt> at the end of the source growing the buffer if necessary.
   procedure Append (Source   : in out Builder;
                     New_Item : in Input);

   --  Append the <tt>New_Item</tt> at the end of the source growing the buffer if necessary.
   procedure Append (Source   : in out Builder;
                     New_Item : in Element_Type);

   --  Clear the source freeing any storage allocated for the buffer.
   procedure Clear (Source : in out Builder);

   --  Iterate over the buffer content calling the <tt>Process</tt> procedure with each
   --  chunk.
   procedure Iterate (Source  : in Builder;
                      Process : not null access procedure (Chunk : in Input));

   --  Get the buffer content as an array.
   function To_Array (Source : in Builder) return Input;

private

   pragma Inline (Length);
   pragma Inline (Iterate);

   type Block;
   type Block_Access is access all Block;

   type Block (Len : Positive) is limited record
      Next_Block : Block_Access;
      Last       : Natural := 0;
      Content    : Input (1 .. Len);
   end record;

   type Builder (Len : Positive) is new Ada.Finalization.Limited_Controlled with record
      Current    : Block_Access;
      Block_Size : Positive := Chunk_Size;
      Length     : Natural  := 0;
      First      : aliased Block (Len);
   end record;
   pragma Finalize_Storage_Only (Builder);

   --  Setup the builder.
   overriding
   procedure Initialize (Source : in out Builder);

   --  Finalize the builder releasing the storage.
   overriding
   procedure Finalize (Source : in out Builder);

end Util.Texts.Builders;