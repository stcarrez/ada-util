-----------------------------------------------------------------------
--  util-texts-builders -- Text builder
--  Copyright (C) 2013, 2017, 2021, 2022 Stephane Carrez
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
private with Ada.Finalization;

--  == Text Builders ==
--  The `Util.Texts.Builders` generic package was designed to provide string builders.
--  The interface was designed to reduce memory copies as much as possible.
--
--    * The `Builder` type holds a list of chunks into which texts are appended.
--    * The builder type holds an initial chunk whose capacity is defined when the builder
--      instance is declared.
--    * There is only an `Append` procedure which allows to append text to the builder.
--      This is the only time when a copy is made.
--    * The package defines the `Iterate` operation that allows to get the content
--      collected by the builder.  When using the `Iterate` operation, no copy is
--      performed since chunks data are passed passed by reference.
--    * The type is limited to forbid copies of the builder instance.
--
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
--  To get the content collected in the builder instance, write a procedure that receives
--  the chunk data as parameter:
--
--    procedure Collect (Item : in String) is ...
--
--  And use the `Iterate` operation:
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

   --  Append in `Into` builder the `Content` builder starting at `From` position
   --  and the up to and including the `To` position.
   procedure Append (Into     : in out Builder;
                     Content  : in Builder;
                     From     : in Positive;
                     To       : in Positive);

   generic
      with procedure Process (Content : in out Input; Last : out Natural);
   procedure Inline_Append (Source  : in out Builder);

   --  Clear the source freeing any storage allocated for the buffer.
   procedure Clear (Source : in out Builder);

   --  Iterate over the buffer content calling the <tt>Process</tt> procedure with each
   --  chunk.
   procedure Iterate (Source  : in Builder;
                      Process : not null access procedure (Chunk : in Input));

   generic
      with procedure Process (Content : in Input);
   procedure Inline_Iterate (Source  : in Builder);

   generic
      with procedure Process (Content : in out Input);
   procedure Inline_Update (Source : in out Builder);

   --  Get the buffer content as an array.
   function To_Array (Source : in Builder) return Input;

   --  Return the content starting from the tail and up to <tt>Length</tt> items.
   function Tail (Source : in Builder;
                  Length : in Natural) return Input;

   --  Get the element at the given position.
   function Element (Source   : in Builder;
                     Position : in Positive) return Element_Type;

   --  Find the position of some content by running the `Index` function.
   --  The `Index` function is called with chunks starting at the given position and
   --  until it returns a positive value or we reach the last chunk.  It must return
   --  the found position in the chunk.
   generic
      with function Index (Content : in Input) return Natural;
   function Find (Source   : in Builder;
                  Position : in Positive) return Natural;

   --  Call the <tt>Process</tt> procedure with the full buffer content, trying to avoid
   --  secondary stack copies as much as possible.
   generic
      with procedure Process (Content : in Input);
   procedure Get (Source : in Builder);

   --  Format the message and replace occurrences of argument patterns by
   --  their associated value.
   --  Returns the formatted message in the stream
   generic
      type Value is limited private;
      type Value_List is array (Positive range <>) of Value;
      with procedure Append (Input : in out Builder; Item : in Value);
   procedure Format (Into      : in out Builder;
                     Message   : in Input;
                     Arguments : in Value_List);

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
