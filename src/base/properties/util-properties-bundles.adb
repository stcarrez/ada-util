-----------------------------------------------------------------------
--  util-properties-bundles -- Generic name/value property management
--  Copyright (C) 2001 - 2020, 2022 Stephane Carrez
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
with Ada.Unchecked_Conversion;
with Ada.Directories;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Util.Log.Loggers;
with Util.Files;
with Util.Concurrent.Counters;
package body Util.Properties.Bundles is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Properties.Bundles");

   use Implementation;

   procedure Free is
     new Ada.Unchecked_Deallocation (Manager'Class,
                                     Bundle_Manager_Access);

   --  Implementation of the Bundle
   --  (this allows to decouples the implementation from the API)
   package Interface_P is

      package PropertyList is new Ada.Containers.Vectors
        (Element_Type => Util.Properties.Manager_Access,
         Index_Type => Natural,
           "=" => "=");

      type Manager is limited new Util.Properties.Implementation.Manager with record
         List   : PropertyList.Vector;
         Props  : aliased Util.Properties.Manager;
         Count  : Util.Concurrent.Counters.Counter := Util.Concurrent.Counters.ONE;
         Shared : Boolean := False;
      end record;

      --  Get the value identified by the name.
      --  If the name cannot be found, the method should return the Null object.
      overriding
      function Get_Value (From : in Manager;
                          Name : in String) return Util.Beans.Objects.Object;

      --  Set the value identified by the name.
      --  If the map contains the given name, the value changed.
      --  Otherwise name is added to the map and the value associated with it.
      overriding
      procedure Set_Value (From  : in out Manager;
                           Name  : in String;
                           Value : in Util.Beans.Objects.Object);

      --  Returns TRUE if the property exists.
      overriding
      function Exists (Self : in Manager;
                       Name : in String)
                       return Boolean;

      --  Remove the property given its name.
      overriding
      procedure Remove (Self : in out Manager;
                        Name : in String);

      --  Iterate over the properties and execute the given procedure passing the
      --  property name and its value.
      overriding
      procedure Iterate (Self    : in Manager;
                         Process : access procedure (Name : in String;
                                                     Item : in Util.Beans.Objects.Object));

      procedure Load_Properties (Self : in out Manager;
                                 File : in String);

      --  Deep copy of properties stored in 'From' to 'To'.
      overriding
      function Create_Copy (Self : in Manager)
                           return Util.Properties.Implementation.Manager_Access;

      procedure Add_Bundle (Self : in out Manager;
                            Props : in Util.Properties.Manager_Access);

      package Shared_Implementation is
        new Implementation.Shared_Implementation (Manager);

      subtype Shared_Manager is Shared_Implementation.Manager;

      function Allocate_Property return Implementation.Shared_Manager_Access is
        (new Shared_Manager);

      --  Create a property implementation if there is none yet.
      procedure Check_And_Create_Impl is
        new Implementation.Create (Allocator => Allocate_Property);

   end Interface_P;

   procedure Add_Bundle (Self  : in out Manager;
                         Props : in Manager_Access) is
   begin
      Interface_P.Manager'Class (Self.Impl.all).Add_Bundle (Props);
   end Add_Bundle;

   overriding
   procedure Initialize (Object : in out Manager) is
   begin
      Interface_P.Check_And_Create_Impl (Object);
   end Initialize;

   overriding
   procedure Adjust (Object : in out Manager) is
   begin
      Interface_P.Check_And_Create_Impl (Object);
      Object.Impl.Adjust;
   end Adjust;

   --  ------------------------------
   --  Initialize the bundle factory and specify where the property files are stored.
   --  ------------------------------
   procedure Initialize (Factory : in out Loader;
                         Path    : in String) is
   begin
      Log.Info ("Initialize bundle factory to load from {0}", Path);

      Factory.Path := To_Unbounded_String (Path);
   end Initialize;

   --  ------------------------------
   --  Load the bundle with the given name and for the given locale name.
   --  ------------------------------
   procedure Load_Bundle (Factory : in out Loader;
                          Name    : in String;
                          Locale  : in String;
                          Bundle  : out Manager'Class) is
      Found : Boolean := False;
   begin
      Log.Info ("Load bundle {0} for language {1}", Name, Locale);

      Find_Bundle (Factory, Name, Locale, Bundle, Found);
      if not Found then
         Load_Bundle (Factory, Name, Found);
         if not Found then
            Log.Error ("Bundle {0} not found", Name);
            raise NO_BUNDLE with "No bundle '" & Name & "'";
         end if;
         Find_Bundle (Factory, Name, Locale, Bundle, Found);
         if not Found then
            Log.Error ("Bundle {0} not found", Name);
            raise NO_BUNDLE with "No bundle '" & Name & "'";
         end if;
      end if;
   end Load_Bundle;

   --  ------------------------------
   --  Find the bundle with the given name and for the given locale name.
   --  ------------------------------
   procedure Find_Bundle (Factory : in out Loader;
                          Name    : in String;
                          Locale  : in String;
                          Bundle  : out Manager'Class;
                          Found   : out Boolean) is
      use Ada.Strings;

      Loc_Name : constant String := '_' & Locale;
      Last_Pos : Integer := Loc_Name'Last;
   begin
      Log.Debug ("Looking for bundle {0} and language {1}", Name, Locale);

      Found := False;
      Factory.Lock.Read;
      declare
         Pos : Bundle_Map.Cursor;
      begin
         while Last_Pos + 1 >= Loc_Name'First loop
            declare
               Bundle_Name : aliased constant String
                 := Name & Loc_Name (Loc_Name'First .. Last_Pos);
            begin
               Log.Debug ("Searching for {0}", Bundle_Name);
               Pos := Factory.Bundles.Find (Bundle_Name'Unrestricted_Access);
               if Bundle_Map.Has_Element (Pos) then
                  Bundle.Finalize;
                  Bundle.Impl := Bundle_Map.Element (Pos).Impl;
                  Bundle.Impl.Adjust;
                  Found := True;
                  exit;
               end if;
            end;
            if Last_Pos > Loc_Name'First then
               Last_Pos := Fixed.Index (Loc_Name, "_", Last_Pos - 1, Backward) - 1;
            else
               Last_Pos := Last_Pos - 1;
            end if;
         end loop;
      exception
         when others =>
            Factory.Lock.Release_Read;
            raise;
      end;
      Factory.Lock.Release_Read;
   end Find_Bundle;

   --  ------------------------------
   --  Load the bundle with the given name and for the given locale name.
   --  ------------------------------
   procedure Load_Bundle (Factory : in out Loader;
                          Name    : in String;
                          Found   : out Boolean) is
      use Ada.Directories;
      use Ada.Strings;
      use Util.Strings;
      use Ada.Containers;
      use Util.Strings.String_Set;
      use Bundle_Map;

      procedure Process_File (Name      : in String;
                              File_Path : in String;
                              Done      : out Boolean);

      Path    : constant String := To_String (Factory.Path);
      Pattern : constant String := Name & "*.properties";
      Names   : Util.Strings.String_Set.Set;

      procedure Process_File (Name      : in String;
                              File_Path : in String;
                              Done      : out Boolean) is
         subtype Cursor is Bundle_Map.Cursor;

         Base_Name   : aliased constant String := Name (Name'First .. Name'Last - 11);
         Pos         : constant Cursor := Factory.Bundles.Find (Base_Name'Unchecked_Access);
         Bundle_Name : Name_Access;
         Bundle      : Bundle_Manager_Access;
      begin
         Log.Info ("Loading file {0}", File_Path);

         if Bundle_Map.Has_Element (Pos) then
            Bundle := Bundle_Map.Element (Pos);
         else
            Bundle := new Manager;
            Bundle_Name := new String '(Base_Name);
            Factory.Bundles.Include (Key => Bundle_Name, New_Item => Bundle);
            Names.Insert (Bundle_Name);
         end if;
         Interface_P.Manager'Class (Bundle.Impl.all).Load_Properties (File_Path);
         Found := True;
         Done := False;
      end Process_File;

   begin
      Log.Info ("Reading bundle {1} in directory {0}", Path, Name);

      Found := False;
      Factory.Lock.Write;

      begin
         Util.Files.Iterate_Files_Path (Pattern => Pattern,
                                        Path    => Path,
                                        Process => Process_File'Access,
                                        Going   => Ada.Strings.Backward);

         --  Link the property files to implement the localization default rules.
         while Names.Length > 0 loop
            declare
               Name_Pos    : String_Set.Cursor := Names.First;
               Bundle_Name : constant Name_Access := String_Set.Element (Name_Pos);
               Idx         : Natural := Fixed.Index (Bundle_Name.all, "_", Backward);
               Bundle_Pos  : constant Bundle_Map.Cursor := Factory.Bundles.Find (Bundle_Name);
               Bundle      : constant Bundle_Manager_Access := Element (Bundle_Pos);
            begin
               Names.Delete (Name_Pos);

               --  Associate the property bundle to the first existing parent
               --  Ex:  message_fr_CA -> message_fr
               --       message_fr_CA -> message
               while Idx > 0 loop
                  declare
                     Name : aliased constant String
                       := Bundle_Name (Bundle_Name'First .. Idx - 1);
                     Pos : constant Bundle_Map.Cursor
                       := Factory.Bundles.Find (Name'Unchecked_Access);
                  begin
                     if Bundle_Map.Has_Element (Pos) then
                        Bundle.Add_Bundle (Bundle_Map.Element (Pos).all'Access);
                        Idx := 0;
                     else
                        Idx := Fixed.Index (Bundle_Name.all, "_", Idx - 1, Backward);
                     end if;
                  end;
               end loop;
            end;
         end loop;

      exception
         when others =>
            Factory.Lock.Release_Write;
            raise;
      end;
      Factory.Lock.Release_Write;

   exception
      when Name_Error =>
         Log.Error ("Cannot read directory: {0}", Path);

   end Load_Bundle;

   --  Implementation of the Bundle
   --  (this allows to decouples the implementation from the API)
   package body Interface_P is
      use PropertyList;

      --  Get the value identified by the name.
      --  If the name cannot be found, the method should return the Null object.
      overriding
      function Get_Value (From : in Manager;
                          Name : in String) return Util.Beans.Objects.Object is
         Result : Util.Beans.Objects.Object := From.Props.Get_Value (Name);
      begin
         if Util.Beans.Objects.Is_Null (Result) then
            declare
               Iter : Cursor := From.List.First;
            begin
               while Has_Element (Iter) loop
                  Result := Element (Iter).all.Get_Value (Name);
                  exit when not Util.Beans.Objects.Is_Null (Result);
                  Iter := Next (Iter);
               end loop;
            end;
         end if;
         return Result;
      end Get_Value;

      --  Set the value identified by the name.
      --  If the map contains the given name, the value changed.
      --  Otherwise name is added to the map and the value associated with it.
      overriding
      procedure Set_Value (From  : in out Manager;
                           Name  : in String;
                           Value : in Util.Beans.Objects.Object) is
      begin
         raise NOT_WRITEABLE with "Bundle is readonly";
      end Set_Value;

      --  ------------------------------
      --  Returns TRUE if the property exists.
      --  ------------------------------
      overriding
      function Exists (Self : in Manager; Name : in String)
                      return Boolean is
         Iter : Cursor := Self.List.First;
      begin
         if Self.Props.Exists (Name) then
            return True;
         end if;
         while Has_Element (Iter) loop
            if Element (Iter).Exists (Name) then
               return True;
            end if;
            Iter := Next (Iter);
         end loop;
         return False;
      end Exists;

      procedure Load_Properties (Self : in out Manager;
                                 File : in String) is
      begin
         Self.Props.Load_Properties (File);
      end Load_Properties;

      --  ------------------------------
      --  Remove the property given its name.
      --  ------------------------------
      overriding
      procedure Remove (Self : in out Manager;
                        Name : in String) is
      begin
         raise NOT_WRITEABLE with "Bundle is readonly";
      end Remove;

      --  Iterate over the properties and execute the given procedure passing the
      --  property name and its value.
      overriding
      procedure Iterate (Self    : in Manager;
                         Process : access procedure (Name : in String;
                                                     Item : in Util.Beans.Objects.Object)) is
      begin
         raise Program_Error with "Iterate is not implemented on Bundle";
      end Iterate;

      --  ------------------------------
      --  Deep copy of properties stored in 'From' to 'To'.
      --  ------------------------------
      overriding
      function Create_Copy (Self : in Manager)
                            return Util.Properties.Implementation.Manager_Access is
         pragma Unreferenced (Self);
      begin
         return null;
      end Create_Copy;

      procedure Add_Bundle (Self  : in out Manager;
                            Props : in Util.Properties.Manager_Access) is
      begin
         Self.List.Append (Props);
      end Add_Bundle;

   end Interface_P;

   --  ------------------------------
   --  Clear the bundle cache
   --  ------------------------------
   procedure Clear_Cache (Factory : in out Loader) is
      use Util.Strings;
      use Bundle_Map;

      function To_String_Access is
        new Ada.Unchecked_Conversion (Source => Util.Strings.Name_Access,
                                      Target => Ada.Strings.Unbounded.String_Access);

   begin
      Log.Info ("Clearing bundle cache");

      Factory.Lock.Write;
      loop
         declare
            Pos  : Bundle_Map.Cursor := Factory.Bundles.First;
            Name : Ada.Strings.Unbounded.String_Access;
            Node : Bundle_Manager_Access;
         begin
            exit when not Has_Element (Pos);
            Node := Element (Pos);
            Name := To_String_Access (Key (Pos));
            Factory.Bundles.Delete (Pos);
            Free (Node);
            Free (Name);
         end;
      end loop;
      Factory.Lock.Release_Write;
   end Clear_Cache;

   --  ------------------------------
   --  Finalize the bundle loader and clear the cache
   --  ------------------------------
   overriding
   procedure Finalize (Factory : in out Loader) is
   begin
      Clear_Cache (Factory);
   end Finalize;

end Util.Properties.Bundles;
