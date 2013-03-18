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
with Ada.Unchecked_Deallocation;

package body Util.Texts.Builders is

   --  ------------------------------
   --  Get the length of the item builder.
   --  ------------------------------
   function Length (Source : in Builder) return Natural is
   begin
      return Source.Length;
   end Length;

   --  ------------------------------
   --  Get the capacity of the builder.
   --  ------------------------------
   function Capacity (Source : in Builder) return Natural is
      B : constant Block_Access := Source.Current;
   begin
      return Source.Length + B.Len - B.Last;
   end Capacity;

   --  ------------------------------
   --  Get the builder block size.
   --  ------------------------------
   function Block_Size (Source : in Builder) return Positive is
   begin
      return Source.Block_Size;
   end Block_Size;

   --  ------------------------------
   --  Set the block size for the allocation of next chunks.
   --  ------------------------------
   procedure Set_Block_Size (Source : in out Builder;
                             Size   : in Positive) is
   begin
      Source.Block_Size := Size;
   end Set_Block_Size;

   --  ------------------------------
   --  Append the <tt>New_Item</tt> at the end of the source growing the buffer if necessary.
   --  ------------------------------
   procedure Append (Source   : in out Builder;
                     New_Item : in Input) is
      B     : Block_Access := Source.Current;
      Start : Natural := New_Item'First;
      Last  : constant Natural := New_Item'Last;
   begin
      while Start <= Last loop
         declare
            Space : Natural := B.Len - B.Last;
            Size  : constant Natural := Last - Start + 1;
         begin
            if Space > Size then
               Space := Size;
            elsif Space = 0 then
               if Size > Source.Block_Size then
                  B.Next_Block := new Block (Size);
               else
                  B.Next_Block := new Block (Source.Block_Size);
               end if;
               B := B.Next_Block;
               Source.Current := B;
               if B.Len > Size then
                  Space := Size;
               else
                  Space  := B.Len;
               end if;
            end if;
            B.Content (B.Last + 1 .. B.Last + Space) := New_Item (Start .. Start + Space - 1);
            Source.Length := Source.Length + Space;
            B.Last := B.Last + Space;
            Start  := Start + Space;
         end;
      end loop;
   end Append;

   --  ------------------------------
   --  Append the <tt>New_Item</tt> at the end of the source growing the buffer if necessary.
   --  ------------------------------
   procedure Append (Source   : in out Builder;
                     New_Item : in Element_Type) is
      B     : Block_Access := Source.Current;
   begin
      if B.Len = B.Last then
         B.Next_Block := new Block (Source.Block_Size);
         B := B.Next_Block;
         Source.Current := B;
      end if;
      Source.Length := Source.Length + 1;
      B.Last := B.Last + 1;
      B.Content (B.Last) := New_Item;
   end Append;

   --  ------------------------------
   --  Clear the source freeing any storage allocated for the buffer.
   --  ------------------------------
   procedure Clear (Source : in out Builder) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Block, Name => Block_Access);
      Current, Next : Block_Access;
   begin
      Next := Source.First.Next_Block;
      while Next /= null loop
         Current := Next;
         Next    := Current.Next_Block;
         Free (Current);
      end loop;
      Source.First.Next_Block := null;
      Source.First.Last       := 0;
      Source.Current          := Source.First'Unchecked_Access;
      Source.Length           := 0;
   end Clear;

   --  ------------------------------
   --  Iterate over the buffer content calling the <tt>Process</tt> procedure with each
   --  chunk.
   --  ------------------------------
   procedure Iterate (Source  : in Builder;
                      Process : not null access procedure (Chunk : in Input)) is
   begin
      if Source.First.Last > 0 then
         Process (Source.First.Content (1 .. Source.First.Last));
         declare
            B : Block_Access := Source.First.Next_Block;
         begin
            while B /= null loop
               Process (B.Content (1 .. B.Last));
               B := B.Next_Block;
            end loop;
         end;
      end if;
   end Iterate;

   --  ------------------------------
   --  Get the buffer content as an array.
   --  ------------------------------
   function To_Array (Source : in Builder) return Input is
      Result : Input (1 .. Source.Length);
   begin
      if Source.First.Last > 0 then
         declare
            Pos : Positive := Source.First.Last;
            B   : Block_Access := Source.First.Next_Block;
         begin
            Result (1 .. Pos) := Source.First.Content (1 .. Pos);
            while B /= null loop
               Result (Pos + 1 .. Pos + B.Last) := B.Content (1 .. B.Last);
               Pos := Pos + B.Last;
               B   := B.Next_Block;
            end loop;
         end;
      end if;
      return Result;
   end To_Array;

   --  ------------------------------
   --  Setup the builder.
   --  ------------------------------
   overriding
   procedure Initialize (Source : in out Builder) is
   begin
      Source.Current := Source.First'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   --  Finalize the builder releasing the storage.
   --  ------------------------------
   overriding
   procedure Finalize (Source : in out Builder) is
   begin
      Clear (Source);
   end Finalize;

end Util.Texts.Builders;