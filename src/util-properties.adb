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

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;
with Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;
with Util.Beans.Objects.Maps;
package body Util.Properties is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded.Text_IO;
   use Interface_P;
   use Util.Beans.Objects;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Interface_P.Manager'Class,
                                     Name   => Interface_P.Manager_Access);

   Trim_Chars : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" " & ASCII.HT);

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
   procedure Remove (Self : in out Property_Map;
                     Name : in String);

   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   overriding
   procedure Iterate (Self    : in Property_Map;
                      Process : access procedure (Name : in String;
                                                  Item : in Util.Beans.Objects.Object));

   --  Deep copy of properties stored in 'From' to 'To'.
   overriding
   function Create_Copy (Self : in Property_Map)
                         return Interface_P.Manager_Access;

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
   procedure Remove (Self : in out Property_Map;
                     Name : in String) is
   begin
      Self.Props.Delete (Name);
   end Remove;

   --  ------------------------------
   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   --  ------------------------------
   overriding
   procedure Iterate (Self    : in Property_Map;
                      Process : access procedure (Name : in String;
                                                  Item : in Util.Beans.Objects.Object)) is
      Iter : Util.Beans.Objects.Maps.Cursor := Self.Props.First;
   begin
      while Util.Beans.Objects.Maps.Has_Element (Iter) loop
         Util.Beans.Objects.Maps.Query_Element (Iter, Process);
         Util.Beans.Objects.Maps.Next (Iter);
      end loop;
   end Iterate;

   --  ------------------------------
   --  Deep copy of properties stored in 'From' to 'To'.
   --  ------------------------------
   overriding
   function Create_Copy (Self : in Property_Map)
                         return Interface_P.Manager_Access is
      Result : constant Property_Map_Access := new Property_Map;
   begin
      --  SCz 2017-07-20: the map assignment is buggy on GNAT 2016 because the copy of the
      --  object also copies the internal Lock and Busy flags which makes the target copy
      --  unusable because the Lock and Busy flag prevents modifications.  Instead of the
      --  Ada assignment, we use the Assign procedure which makes the deep copy of the map.
      --  Result.Props := Self.Props;
      Result.Props.Assign (Self.Props);
      return Result.all'Access;
   end Create_Copy;

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
                    Name : in Unbounded_String) return Boolean is
   begin
      --  There is not yet an implementation, no property
      return Self.Impl /= null and then Self.Impl.Exists (-Name);
   end Exists;

   function Get (Self : in Manager'Class;
                 Name : in String) return Unbounded_String is
      Value : constant Util.Beans.Objects.Object := Self.Get_Value (Name);
   begin
      if Util.Beans.Objects.Is_Null (Value) then
         raise NO_PROPERTY with "No property: '" & Name & "'";
      end if;

      return To_Unbounded_String (Value);
   end Get;

   function Get (Self : in Manager'Class;
                 Name : in Unbounded_String) return Unbounded_String is
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
                 Name : in Unbounded_String) return String is
   begin
      return Self.Get (-Name);
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

   --  ------------------------------
   --  Returns a property manager that is associated with the given name.
   --  Raises NO_PROPERTY if there is no such property manager or if a property exists
   --  but is not a property manager.
   --  ------------------------------
   function Get (Self : in Manager'Class;
                 Name : in String) return Manager is
      Value : constant Util.Beans.Objects.Object := Self.Get_Value (Name);
      Bean  : constant access Util.Beans.Basic.Readonly_Bean'Class
        := Util.Beans.Objects.To_Bean (Value);
   begin
      if Bean = null or else not (Bean.all in Manager'Class) then
         raise NO_PROPERTY with "No property '" & Name & "'";
      end if;
      return Manager (Bean.all);
   end Get;

   --  ------------------------------
   --  Create a property manager and associated it with the given name.
   --  ------------------------------
   function Create (Self : in out Manager'Class;
                    Name : in String) return Manager is
      Result : Manager_Access;
   begin
      Check_And_Create_Impl (Self);

      --  Create the property manager and make it shared so that it can be populated by
      --  the caller.
      Result := new Manager;
      Check_And_Create_Impl (Result.all);
      Result.Impl.Shared := True;
      Self.Impl.Set_Value (Name, Util.Beans.Objects.To_Object (Result.all'Access));
      return Manager (Result.all);
   end Create;

   procedure Check_And_Create_Impl (Self : in out Manager) is
   begin
      if Self.Impl = null then
         Self.Impl := new Property_Map;
         Util.Concurrent.Counters.Increment (Self.Impl.Count);
      elsif not Self.Impl.Shared and Util.Concurrent.Counters.Value (Self.Impl.Count) > 1 then
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
                  Item : in Unbounded_String) is
   begin
      Self.Set_Value (Name, To_Object (Item));
   end Set;

   --  ------------------------------
   --  Set the value of the property.  The property is created if it
   --  does not exists.
   --  ------------------------------
   procedure Set (Self : in out Manager'Class;
                  Name : in Unbounded_String;
                  Item : in Unbounded_String) is
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
      Remove (Self.Impl.all, Name);
   end Remove;

   --  ------------------------------
   --  Remove the property given its name.
   --  ------------------------------
   procedure Remove (Self : in out Manager'Class;
                     Name : in Unbounded_String) is
   begin
      Self.Remove (-Name);
   end Remove;

   --  ------------------------------
   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   --  ------------------------------
   procedure Iterate (Self    : in Manager'Class;
                      Process : access procedure (Name : in String;
                                                  Item : in Util.Beans.Objects.Object)) is
   begin
      if Self.Impl /= null then
         Self.Impl.Iterate (Process);
      end if;
   end Iterate;

   --  ------------------------------
   --  Get the property manager represented by the item value.
   --  Raise the Conversion_Error exception if the value is not a property manager.
   --  ------------------------------
   function To_Manager (Item : in Value) return Manager is
      Bean  : constant access Util.Beans.Basic.Readonly_Bean'Class
        := Util.Beans.Objects.To_Bean (Item);
   begin
      if Bean = null or else not (Bean.all in Manager'Class) then
         raise Util.Beans.Objects.Conversion_Error;
      end if;
      return Manager (Bean.all);
   end To_Manager;

   --  ------------------------------
   --  Returns True if the item value represents a property manager.
   --  ------------------------------
   function Is_Manager (Item : in Value) return Boolean is
      Bean  : constant access Util.Beans.Basic.Readonly_Bean'Class
        := Util.Beans.Objects.To_Bean (Item);
   begin
      return Bean /= null and then (Bean.all in Manager'Class);
   end Is_Manager;

   --  ------------------------------
   --  Collect the name of the properties defined in the manager.
   --  When a prefix is specified, only the properties starting with the prefix are
   --  returned.
   --  ------------------------------
   procedure Get_Names (Self   : in Manager;
                        Into   : in out Util.Strings.Vectors.Vector;
                        Prefix : in String := "") is
      procedure Process (Name : in String;
                         Item : in Util.Beans.Objects.Object);

      procedure Process (Name : in String;
                         Item : in Util.Beans.Objects.Object) is
         pragma Unreferenced (Item);
      begin
         if Prefix'Length = 0 or else Ada.Strings.Fixed.Index (Name, Prefix) = 1 then
            Into.Append (Name);
         end if;
      end Process;
   begin
      if Self.Impl /= null then
         Self.Impl.Iterate (Process'Access);
      end if;
   end Get_Names;

   overriding
   procedure Adjust (Object : in out Manager) is
   begin
      if Object.Impl /= null then
         Util.Concurrent.Counters.Increment (Object.Impl.Count);
      end if;
   end Adjust;

   overriding
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

   procedure Load_Properties (Self   : in out Manager'Class;
                              File   : in File_Type;
                              Prefix : in String := "";
                              Strip  : in Boolean := False) is
      pragma Unreferenced (Strip);
      Line    : Unbounded_String;
      Name    : Unbounded_String;
      Value   : Unbounded_String;
      Current : Manager;
      Pos     : Natural;
      Len     : Natural;
      Old_Shared : Boolean;
   begin
      Check_And_Create_Impl (Self);
      Current := Manager (Self);
      Old_Shared := Current.Impl.Shared;
      Current.Impl.Shared := True;
      while not End_Of_File (File) loop
         Line := Get_Line (File);
         Len  := Length (Line);
         if Len /= 0 then
            case Element (Line, 1) is
               when '!' | '#' =>
                  null;

               when '[' =>
                  Pos := Index (Line, "]");
                  if Pos > 0 then
                     Current := Self.Create (To_String (Unbounded_Slice (Line, 2, Pos - 1)));
                  end if;

               when others =>
                  Pos := Index (Line, "=");
                  if Pos > 0 and then Prefix'Length > 0 and then Index (Line, Prefix) = 1 then
                     Name  := Unbounded_Slice (Line, Prefix'Length + 1, Pos - 1);
                     Value := Tail (Line, Len - Pos);
                     Trim (Name, Trim_Chars, Trim_Chars);
                     Trim (Value, Trim_Chars, Trim_Chars);
                     Current.Set (Name, Value);

                  elsif Pos > 0 and Prefix'Length = 0 then
                     Name  := Head (Line, Pos - 1);
                     Value := Tail (Line, Len - Pos);
                     Trim (Name, Trim_Chars, Trim_Chars);
                     Trim (Value, Trim_Chars, Trim_Chars);
                     Current.Set (Name, Value);

                  end if;

            end case;
         end if;
      end loop;
      Self.Impl.Shared := Old_Shared;

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
      procedure Save_Property (Name : in String;
                               Item : in Util.Beans.Objects.Object);

      Tmp : constant String := Path & ".tmp";
      F : File_Type;

      procedure Save_Property (Name : in String;
                               Item : in Util.Beans.Objects.Object) is
      begin
         if Prefix'Length > 0 then
            if Name'Length < Prefix'Length then
               return;
            end if;
            if Name (Name'First .. Prefix'Length - 1) /= Prefix then
               return;
            end if;
         end if;
         Put (F, Name);
         Put (F, "=");
         Put (F, Util.Beans.Objects.To_String (Item));
         New_Line (F);
      end Save_Property;

      --  Rename a file (the Ada.Directories.Rename does not allow to use the
      --  Unix atomic file rename!)
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
      procedure Process (Name : in String;
                         Value : in Util.Beans.Objects.Object);

      procedure Process (Name  : in String;
                         Value : in Util.Beans.Objects.Object) is
      begin
         if Prefix'Length > 0 then
            if Name'Length < Prefix'Length then
               return;
            end if;
            if Name (Name'First .. Name'First + Prefix'Length - 1) /= Prefix then
               return;
            end if;
            if Strip then
               Self.Set_Value (Name (Name'First + Prefix'Length .. Name'Last), Value);
               return;
            end if;
         end if;
         Self.Set_Value (Name, Value);
      end Process;

   begin
      From.Iterate (Process'Access);
   end Copy;

end Util.Properties;
