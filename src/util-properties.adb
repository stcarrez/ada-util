-----------------------------------------------------------------------
--  properties -- Generic name/value property management
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010 Stephane Carrez
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

with Util.Properties.Factories;
with Ada.Strings.Unbounded.Text_IO;
package body Util.Properties is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded.Text_IO;
   use Interface_P;

   function Exists (Self : in Manager'Class;
                    Name : in String) return Boolean is
   begin
      --  There is not yet an implementation, no property
      if Self.Impl = null then
         return False;
      end if;

      return Exists (Self.Impl.all, +Name);
   end Exists;

   function Exists (Self : in Manager'Class;
                    Name : in Value) return Boolean is
   begin
      --  There is not yet an implementation, no property
      if Self.Impl = null then
         return False;
      end if;

      return Exists (Self.Impl.all, Name);
   end Exists;

   function Get (Self : in Manager'Class;
                 Name : in String) return Value is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY with "No property: '" & Name & "'";
      end if;

      return Get (Self.Impl.all, +Name);
   end Get;

   function Get (Self : in Manager'Class;
                 Name : in Value) return Value is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY with "No property: '" & To_String (Name) & "'";
      end if;

      return Get (Self.Impl.all, Name);
   end Get;

   function Get (Self : in Manager'Class;
                 Name : in String) return String is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY with "No property: '" & Name & "'";
      end if;

      return -Get (Self.Impl.all, +Name);
   end Get;

   function Get (Self : in Manager'Class;
                 Name : in Value) return String is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY;
      end if;

      return -Get (Self.Impl.all, Name);
   end Get;

   function Get (Self : in Manager'Class;
                 Name : in String;
                 Default : in String) return String is
      Prop_Name : constant Value := +Name;
   begin
      if Exists (Self, Prop_Name) then
         return Get (Self, Prop_Name);
      else
         return Default;
      end if;
   end Get;

   procedure Check_And_Create_Impl (Self : in out Manager) is
   begin
      if Self.Impl = null then
         Util.Properties.Factories.Initialize (Self);
         Self.Impl.Count := 1;
      elsif Self.Impl.Count > 1 then
         Self.Impl := Create_Copy (Self.Impl.all);
      end if;
   end Check_And_Create_Impl;

   procedure Insert (Self : in out Manager'Class;
                     Name : in String;
                     Item : in String) is
   begin
      Check_And_Create_Impl (Self);
      Insert (Self.Impl.all, +Name, +Item);
   end Insert;

   --  ------------------------------
   --  Set the value of the property.  The property is created if it
   --  does not exists.
   --  ------------------------------
   procedure Set (Self : in out Manager'Class;
                  Name : in String;
                  Item : in String) is
   begin
      Check_And_Create_Impl (Self);
      Set (Self.Impl.all, +Name, +Item);
   end Set;

   --  ------------------------------
   --  Set the value of the property.  The property is created if it
   --  does not exists.
   --  ------------------------------
   procedure Set (Self : in out Manager'Class;
                  Name : in String;
                  Item : in Value) is
   begin
      Check_And_Create_Impl (Self);
      Set (Self.Impl.all, +Name, Item);
   end Set;

   --  ------------------------------
   --  Set the value of the property.  The property is created if it
   --  does not exists.
   --  ------------------------------
   procedure Set (Self : in out Manager'Class;
                  Name : in Unbounded_String;
                  Item : in Value) is
   begin
      Check_And_Create_Impl (Self);
      Set (Self.Impl.all, Name, Item);
   end Set;

   --  ------------------------------
   --  Remove the property given its name.
   --  ------------------------------
   procedure Remove (Self : in out Manager'Class;
                     Name : in String) is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY;
      end if;
      Remove (Self.Impl.all, +Name);
   end Remove;

   --  ------------------------------
   --  Remove the property given its name.
   --  ------------------------------
   procedure Remove (Self : in out Manager'Class;
                     Name : in Value) is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY;
      end if;
      Remove (Self.Impl.all, Name);
   end Remove;

   function Get_Names (Self : in Manager) return Name_Array is
   begin
      if Self.Impl = null then
         declare
            Empty : Name_Array (1 .. 0);
         begin
            return Empty;
         end;
      else
         return Get_Names (Self.Impl.all);
      end if;
   end Get_Names;

   procedure Adjust (Object : in out Manager) is
   begin
      if Object.Impl /= null then
         Object.Impl.Count := Object.Impl.Count + 1;
      end if;
   end Adjust;

   procedure Finalize (Object : in out Manager) is
   begin
      if Object.Impl /= null then
         Object.Impl.Count := Object.Impl.Count - 1;
         if Object.Impl.Count = 0 then
            Delete (Object.Impl.all, Object.Impl);
         end if;
      end if;
   end Finalize;

   procedure Set_Property_Implementation (Self : in out Manager;
                                          Impl : in Interface_P.Manager_Access) is
   begin
      if Self.Impl = null then
         Self.Impl := Impl;
         Self.Impl.Count := 1;
      end if;
   end Set_Property_Implementation;

   procedure Load_Property (Name  : out Unbounded_String;
                            Value : out Unbounded_String;
                            File  : in File_Type) is
      Line : Unbounded_String;
      Pos  : Natural;
      Len  : Natural;
   begin
      while not End_Of_File (File) loop
         Line := Get_Line (File);
         Len  := Length (Line);
         if Len /= 0 and then Element (Line, 1) /= '#' then
            Pos := Index (Line, "=");
            if Pos > 0 then
               Name  := Head (Line, Pos - 1);
               Value := Tail (Line, Len - Pos);
               return;
            end if;
         end if;
      end loop;
      Name := Null_Unbounded_String;
      Value := Null_Unbounded_String;
   end Load_Property;

   procedure Load_Properties (Self : in out Manager'Class;
                              File : in File_Type) is
      Name, Value : Unbounded_String;
   begin
      loop
         Load_Property (Name, Value, File);
         exit when Name = Null_Unbounded_String;
         Set (Self, To_String (Name), To_String (Value));
      end loop;

   exception
      when End_Error =>
         return;
   end Load_Properties;

   procedure Load_Properties (Self : in out Manager'Class;
                              Path : in String) is
      F : File_Type;
   begin
      Open (F, In_File, Path);
      Load_Properties (Self, F);
      Close (F);
   end Load_Properties;

   --  ------------------------------
   --  Copy the properties from FROM which start with a given prefix.
   --  If the prefix is empty, all properties are copied.
   --  ------------------------------
   procedure Copy (Self : in out Manager'Class;
                   From : in Manager'Class;
                   Prefix : in String := "") is
      Names : constant Name_Array := From.Get_Names;
   begin
      for I in Names'Range loop
         declare
            Name : Unbounded_String renames Names (I);
         begin
            if Prefix'Length = 0 or else Index (Name, Prefix) = 1 then
               Self.Set (Name, From.Get (Name));
            end if;
         end;
      end loop;

   end Copy;

end Util.Properties;
