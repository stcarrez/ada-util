-----------------------------------------------------------------------
--  properties -- Generic name/value property management
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011, 2012, 2014, 2017 Stephane Carrez
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

--  with Util.Properties.Factories;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded.Text_IO;
with Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;
package body Util.Properties is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded.Text_IO;
   use Interface_P;
   use Util.Beans.Objects;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Interface_P.Manager'Class,
                                     Name   => Interface_P.Manager_Access);

   type Property_Map is new Interface_P.Manager with record
      Props : Util.Beans.Objects.Maps.Map_Bean;
   end record;
   type Property_Map_Access is access all Property_Map;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Property_Map;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   overriding
   procedure Set_Value (From  : in out Property_Map;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Returns TRUE if the property exists.
   overriding
   function Exists (Self : in Property_Map;
                    Name : in String)
                    return Boolean;

   --  Remove the property given its name.
   overriding
   procedure Remove (Self : in out Property_Map; Name : in Value);

   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   overriding
   procedure Iterate (Self    : in Property_Map;
                      Process : access procedure (Name, Item : Value));

   --  Deep copy of properties stored in 'From' to 'To'.
   overriding
   function Create_Copy (Self : in Property_Map)
                         return Interface_P.Manager_Access;

   overriding
   function Get_Names (Self   : in Property_Map;
                       Prefix : in String) return Name_Array;

   procedure Load_Property (Name   : out Unbounded_String;
                            Value  : out Unbounded_String;
                            File   : in File_Type;
                            Prefix : in String := "";
                            Strip  : in Boolean := False);

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Property_Map;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return From.Props.Get_Value (Name);
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Property_Map;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      From.Props.Set_Value (Name, Value);
   end Set_Value;

   --  ------------------------------
   --  Returns TRUE if the property exists.
   --  ------------------------------
   overriding
   function Exists (Self : in Property_Map;
                    Name : in String)
                    return Boolean is
   begin
      return Self.Props.Contains (Name);
   end Exists;

   --  ------------------------------
   --  Remove the property given its name.
   --  ------------------------------
   overriding
   procedure Remove (Self : in out Property_Map; Name : in Value) is
   begin
      null;
   end Remove;

   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   overriding
   procedure Iterate (Self    : in Property_Map;
                      Process : access procedure (Name, Item : Value)) is
   begin
      null;
   end Iterate;

   --  Deep copy of properties stored in 'From' to 'To'.
   overriding
   function Create_Copy (Self : in Property_Map)
                         return Interface_P.Manager_Access is
      Result : Property_Map_Access := new Property_Map;
   begin
      Result.Props := Self.Props;
      return Result.all'Access;
   end Create_Copy;

   overriding
   function Get_Names (Self   : in Property_Map;
                       Prefix : in String) return Name_Array is
      N : Name_Array (1 .. 0);
   begin
      return N;
   end Get_Names;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Manager;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Impl = null then
         return Util.Beans.Objects.Null_Object;
      else
         return From.Impl.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Manager;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      Check_And_Create_Impl (From);
      From.Impl.Set_Value (Name, Value);
   end Set_Value;

   function Exists (Self : in Manager'Class;
                    Name : in String) return Boolean is
   begin
      --  There is not yet an implementation, no property
      return Self.Impl /= null and then Self.Impl.Exists (Name);
   end Exists;

   function Exists (Self : in Manager'Class;
                    Name : in Value) return Boolean is
   begin
      --  There is not yet an implementation, no property
      return Self.Impl /= null and then Self.Impl.Exists (-Name);
   end Exists;

   function Get (Self : in Manager'Class;
                 Name : in String) return Value is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY with "No property: '" & Name & "'";
      end if;

      return Value (To_Unbounded_String (Self.Impl.Get_Value (Name)));
   end Get;

   function Get (Self : in Manager'Class;
                 Name : in Value) return Value is
   begin
      return Self.Get (-Name);
   end Get;

   function Get (Self : in Manager'Class;
                 Name : in String) return String is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY with "No property: '" & Name & "'";
      end if;

      return To_String (Self.Impl.Get_Value (Name));
   end Get;

   function Get (Self : in Manager'Class;
                 Name : in Value) return String is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY;
      end if;

      return To_String (Self.Get_Value (-Name));
   end Get;

   function Get (Self : in Manager'Class;
                 Name : in String;
                 Default : in String) return String is
   begin
      if Exists (Self, Name) then
         return Get (Self, Name);
      else
         return Default;
      end if;
   end Get;

   procedure Check_And_Create_Impl (Self : in out Manager) is
   begin
      if Self.Impl = null then
         --  Util.Properties.Factories.Initialize (Self);
         Self.Impl := new Property_Map;
         Util.Concurrent.Counters.Increment (Self.Impl.Count);
      elsif Util.Concurrent.Counters.Value (Self.Impl.Count) > 1 then
         declare
            Old     : Interface_P.Manager_Access := Self.Impl;
            Is_Zero : Boolean;
         begin
            Self.Impl := Create_Copy (Self.Impl.all);
            Util.Concurrent.Counters.Increment (Self.Impl.Count);
            Util.Concurrent.Counters.Decrement (Old.Count, Is_Zero);
            if Is_Zero then
               Free (Old);
            end if;
         end;
      end if;
   end Check_And_Create_Impl;

   --  ------------------------------
   --  Set the value of the property.  The property is created if it
   --  does not exists.
   --  ------------------------------
   procedure Set (Self : in out Manager'Class;
                  Name : in String;
                  Item : in String) is
   begin
      Self.Set_Value (Name, To_Object (Item));
   end Set;

   --  ------------------------------
   --  Set the value of the property.  The property is created if it
   --  does not exists.
   --  ------------------------------
   procedure Set (Self : in out Manager'Class;
                  Name : in String;
                  Item : in Value) is
   begin
      Self.Set_Value (Name, To_Object (Item));
   end Set;

   --  ------------------------------
   --  Set the value of the property.  The property is created if it
   --  does not exists.
   --  ------------------------------
   procedure Set (Self : in out Manager'Class;
                  Name : in Unbounded_String;
                  Item : in Value) is
   begin
      Self.Set_Value (-Name, To_Object (Item));
   end Set;

   --  ------------------------------
   --  Remove the property given its name.
   --  ------------------------------
   procedure Remove (Self : in out Manager'Class;
                     Name : in String) is
   begin
      if Self.Impl = null then
         raise NO_PROPERTY with "No property '" & Name & "'";
      end if;
      Check_And_Create_Impl (Self);
      Remove (Self.Impl.all, +Name);
   end Remove;

   --  ------------------------------
   --  Remove the property given its name.
   --  ------------------------------
   procedure Remove (Self : in out Manager'Class;
                     Name : in Value) is
   begin
      Self.Remove (-Name);
   end Remove;

   --  ------------------------------
   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   --  ------------------------------
   procedure Iterate (Self    : in Manager'Class;
                      Process : access procedure (Name, Item : Value)) is
   begin
      if Self.Impl /= null then
         Self.Impl.Iterate (Process);
      end if;
   end Iterate;

   --  ------------------------------
   --  Return the name of the properties defined in the manager.
   --  When a prefix is specified, only the properties starting with
   --  the prefix are returned.
   --  ------------------------------
   function Get_Names (Self  : in Manager;
                       Prefix : in String := "") return Name_Array is
   begin
      if Self.Impl = null then
         declare
            Empty : Name_Array (1 .. 0);
         begin
            return Empty;
         end;
      else
         return Get_Names (Self.Impl.all, Prefix);
      end if;
   end Get_Names;

   procedure Adjust (Object : in out Manager) is
   begin
      if Object.Impl /= null then
         Util.Concurrent.Counters.Increment (Object.Impl.Count);
      end if;
   end Adjust;

   procedure Finalize (Object : in out Manager) is
      Is_Zero : Boolean;
   begin
      if Object.Impl /= null then
         Util.Concurrent.Counters.Decrement (Object.Impl.Count, Is_Zero);
         if Is_Zero then
            Free (Object.Impl);
         end if;
      end if;
   end Finalize;

   procedure Set_Property_Implementation (Self : in out Manager;
                                          Impl : in Interface_P.Manager_Access) is
   begin
      if Self.Impl = null then
         Self.Impl := Impl;
--           Self.Impl.Count := 1;
      end if;
   end Set_Property_Implementation;

   procedure Load_Property (Name   : out Unbounded_String;
                            Value  : out Unbounded_String;
                            File   : in File_Type;
                            Prefix : in String := "";
                            Strip  : in Boolean := False) is
      pragma Unreferenced (Strip);

      Line : Unbounded_String;
      Pos  : Natural;
      Len  : Natural;
   begin
      while not End_Of_File (File) loop
         Line := Get_Line (File);
         Len  := Length (Line);
         if Len /= 0 and then Element (Line, 1) /= '#' then
            Pos := Index (Line, "=");
            if Pos > 0 and then Prefix'Length > 0 and then Index (Line, Prefix) = 1 then
               Name  := Unbounded_Slice (Line, Prefix'Length + 1, Pos - 1);
               Value := Tail (Line, Len - Pos);
               return;

            elsif Pos > 0 and Prefix'Length = 0 then
               Name  := Head (Line, Pos - 1);
               Value := Tail (Line, Len - Pos);
               return;

            end if;
         end if;
      end loop;
      Name := Null_Unbounded_String;
      Value := Null_Unbounded_String;
   end Load_Property;

   procedure Load_Properties (Self   : in out Manager'Class;
                              File   : in File_Type;
                              Prefix : in String := "";
                              Strip  : in Boolean := False) is
      Name, Value : Unbounded_String;
   begin
      loop
         Load_Property (Name, Value, File, Prefix, Strip);
         exit when Name = Null_Unbounded_String;
         Set (Self, Name, Value);
      end loop;

   exception
      when End_Error =>
         return;
   end Load_Properties;

   procedure Load_Properties (Self   : in out Manager'Class;
                              Path   : in String;
                              Prefix : in String := "";
                              Strip  : in Boolean := False) is
      F : File_Type;
   begin
      Open (F, In_File, Path);
      Load_Properties (Self, F, Prefix, Strip);
      Close (F);
   end Load_Properties;

   --  ------------------------------
   --  Save the properties in the given file path.
   --  ------------------------------
   procedure Save_Properties (Self   : in out Manager'Class;
                              Path   : in String;
                              Prefix : in String := "") is
      procedure Save_Property (Name, Item : in Value);

      Tmp : constant String := Path & ".tmp";
      F : File_Type;

      procedure Save_Property (Name, Item : in Value) is
      begin
         Put (F, Name);
         Put (F, "=");
         Put (F, Item);
         New_Line (F);
      end Save_Property;

      --  Rename a file (the Ada.Directories.Rename does not allow to use the Unix atomic file rename!)
      function Sys_Rename (Oldpath  : in Interfaces.C.Strings.chars_ptr;
                           Newpath  : in Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Sys_Rename, "rename");

      Old_Path : Interfaces.C.Strings.chars_ptr;
      New_Path : Interfaces.C.Strings.chars_ptr;
      Result   : Integer;
   begin
      Create (File => F, Name => Tmp);
      Self.Iterate (Save_Property'Access);
      Close (File => F);

      --  Do a system atomic rename of old file in the new file.
      --  Ada.Directories.Rename does not allow this.
      Old_Path := Interfaces.C.Strings.New_String (Tmp);
      New_Path := Interfaces.C.Strings.New_String (Path);
      Result := Sys_Rename (Old_Path, New_Path);
      Interfaces.C.Strings.Free (Old_Path);
      Interfaces.C.Strings.Free (New_Path);
      if Result /= 0 then
         raise Ada.IO_Exceptions.Use_Error with "Cannot rename file";
      end if;
   end Save_Properties;

   --  ------------------------------
   --  Copy the properties from FROM which start with a given prefix.
   --  If the prefix is empty, all properties are copied.  When <b>Strip</b> is True,
   --  the prefix part is removed from the property name.
   --  ------------------------------
   procedure Copy (Self   : in out Manager'Class;
                   From   : in Manager'Class;
                   Prefix : in String := "";
                   Strip  : in Boolean := False) is
      Names : constant Name_Array := From.Get_Names;
   begin
      for I in Names'Range loop
         declare
            Name : Unbounded_String renames Names (I);
         begin
            if Prefix'Length = 0 or else Index (Name, Prefix) = 1 then
               if Strip and Prefix'Length > 0 then
                  declare
                     S : constant String := Slice (Name, Prefix'Length + 1, Length (Name));
                  begin
                     Self.Set (+(S), From.Get (Name));
                  end;
               else
                  Self.Set (Name, From.Get (Name));
               end if;
            end if;
         end;
      end loop;

   end Copy;

end Util.Properties;
