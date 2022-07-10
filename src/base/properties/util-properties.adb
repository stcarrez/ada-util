-----------------------------------------------------------------------
--  util-properties -- Generic name/value property management
--  Copyright (C) 2001 - 2022 Stephane Carrez
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;
with Util.Files;
with Util.Beans.Objects.Maps;
package body Util.Properties is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded.Text_IO;
   use Implementation;
   use Util.Beans.Objects;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Implementation.Shared_Manager'Class,
                                     Name   => Implementation.Shared_Manager_Access);

   Trim_Chars : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" " & ASCII.HT);

   package body Implementation is

      procedure Create (Self : in out Util.Properties.Manager'Class) is
      begin
         if Self.Impl = null then
            Self.Impl := Allocator;
         elsif Self.Impl.Is_Shared then
            declare
               Old     : Implementation.Shared_Manager_Access := Self.Impl;
               Is_Zero : Boolean;
               Impl    : constant Implementation.Manager_Access := Create_Copy (Self.Impl.all);
            begin
               Self.Impl := Implementation.Shared_Manager'Class (Impl.all)'Access;
               Old.Finalize (Is_Zero);
               if Is_Zero then
                  Free (Old);
               end if;
            end;
         end if;
      end Create;

      procedure Initialize (Self : in out Util.Properties.Manager'Class) is
         Release : Boolean;
      begin
         if Self.Impl /= null then
            Self.Impl.Finalize (Release);
            if Release then
               Free (Self.Impl);
            end if;
         end if;
         Self.Impl := Allocator;
      end Initialize;

      package body Shared_Implementation is

         overriding
         function Is_Shared (Self : in Manager) return Boolean is
         begin
            return not Self.Shared and then Util.Concurrent.Counters.Value (Self.Count) > 1;
         end Is_Shared;

         overriding
         procedure Set_Shared (Self   : in out Manager;
                               Shared : in Boolean) is
         begin
            Self.Shared := Shared;
         end Set_Shared;

         overriding
         procedure Adjust (Self : in out Manager) is
         begin
            Util.Concurrent.Counters.Increment (Self.Count);
         end Adjust;

         overriding
         procedure Finalize (Self    : in out Manager;
                             Release : out Boolean) is
         begin
            Util.Concurrent.Counters.Decrement (Self.Count, Release);
         end Finalize;

      end Shared_Implementation;

   end Implementation;

   type Property_Map is limited new Implementation.Manager with record
      Props  : Util.Beans.Objects.Maps.Map_Bean;
   end record;

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
                         return Implementation.Manager_Access;

   package Shared_Property_Implementation is
      new Implementation.Shared_Implementation (Property_Map);

   subtype Property_Manager is Shared_Property_Implementation.Manager;
   type Property_Manager_Access is access all Property_Manager'Class;

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
                         return Implementation.Manager_Access is
      Result : constant Property_Manager_Access := new Property_Manager;
   begin
      --  SCz 2017-07-20: the map assignment is buggy on GNAT 2016 because the copy of the
      --  object also copies the internal Lock and Busy flags which makes the target copy
      --  unusable because the Lock and Busy flag prevents modifications.  Instead of the
      --  Ada assignment, we use the Assign procedure which makes the deep copy of the map.
      --  Result.Props := Self.Props;
      Result.Props.Assign (Self.Props);
      return Result.all'Access;
   end Create_Copy;

   function Allocate_Property return Implementation.Shared_Manager_Access is
     (new Property_Manager);

   --  Create a property implementation if there is none yet.
   procedure Check_And_Create_Impl is
      new Implementation.Create (Allocator => Allocate_Property);

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

   --  ------------------------------
   --  Returns TRUE if the property manager is empty.
   --  ------------------------------
   function Is_Empty (Self : in Manager'Class) return Boolean is
   begin
      if Self.Impl = null then
         return True;
      else
         return False;
      end if;
   end Is_Empty;

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
      Value : constant Util.Beans.Objects.Object := Self.Get_Value (Name);
   begin
      if Util.Beans.Objects.Is_Null (Value) then
         raise NO_PROPERTY with "No property: '" & Name & "'";
      end if;

      return To_String (Value);
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
      Result.Impl.Set_Shared (True);
      Self.Impl.Set_Value (Name, Util.Beans.Objects.To_Object (Result.all'Access));
      return Manager (Result.all);
   end Create;

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
      if Bean = null then
         raise Util.Beans.Objects.Conversion_Error;
      end if;
      if Bean.all in Util.Beans.Objects.Maps.Map_Bean'Class then
         declare
            Props  : Manager;
            Impl   : Property_Manager_Access;
         begin
            Check_And_Create_Impl (Props);
            Impl := Property_Manager'Class (Props.Impl.all)'Access;
            Impl.Props.Assign (Util.Beans.Objects.Maps.Map_Bean (Bean.all));
            return Props;
         end;
      end if;
      if not (Bean.all in Manager'Class) then
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
         Object.Impl.Adjust;
      end if;
   end Adjust;

   overriding
   procedure Finalize (Object : in out Manager) is
      Is_Zero : Boolean;
   begin
      if Object.Impl /= null then
         Object.Impl.Finalize (Is_Zero);
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
   begin
      Check_And_Create_Impl (Self);
      Current := Manager (Self);
      Current.Impl.Set_Shared (True);
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

                  elsif Pos > 0 and then Prefix'Length = 0 then
                     Name  := Head (Line, Pos - 1);
                     Value := Tail (Line, Len - Pos);
                     Trim (Name, Trim_Chars, Trim_Chars);
                     Trim (Value, Trim_Chars, Trim_Chars);
                     Current.Set (Name, Value);

                  end if;

            end case;
         end if;
      end loop;
      Self.Impl.Set_Shared (False);

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

      Tmp      : constant String := Path & ".tmp";
      F        : File_Type;
      Recurse  : Boolean := False;
      Level    : Natural := 0;
      Ini_Mode : Boolean := False;

      procedure Save_Property (Name : in String;
                               Item : in Util.Beans.Objects.Object) is
         Bean  : constant access Util.Beans.Basic.Readonly_Bean'Class
           := Util.Beans.Objects.To_Bean (Item);
      begin
         if Bean /= null then
            if Recurse then
               New_Line (F);
               Put (F, "[");
               Put (F, Name);
               Put (F, "]");
               New_Line (F);
               Level := Level + 1;
               To_Manager (Item).Iterate (Save_Property'Access);
               Level := Level - 1;
            else
               Ini_Mode := True;
            end if;
         elsif not Recurse or else Level > 0 then
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
         end if;
      end Save_Property;

   begin
      Create (File => F, Name => Tmp);
      Self.Iterate (Save_Property'Access);
      if Ini_Mode then
         Recurse := True;
         Self.Iterate (Save_Property'Access);
      end if;
      Close (File => F);

      --  Do a system atomic rename of old file in the new file.
      --  Ada.Directories.Rename does not allow this.
      Util.Files.Rename (Tmp, Path);
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
