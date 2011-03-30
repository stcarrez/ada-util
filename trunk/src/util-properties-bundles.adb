-----------------------------------------------------------------------
--  properties -- Generic name/value property management
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011 Stephane Carrez
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
package body Util.Properties.Bundles is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Util.Properties.Bundles");

   procedure Free is
     new Ada.Unchecked_Deallocation (Manager'Class,
                                     Bundle_Manager_Access);

   --  Implementation of the Bundle
   --  (this allows to decouples the implementation from the API)
   package Interface_P is

      type Manager is new Util.Properties.Interface_P.Manager with private;
      type Manager_Object_Access is access all Manager;

      --  Returns TRUE if the property exists.
      function Exists (Self : in Manager; Name : in Value)
                       return Boolean;

      --  Returns the property value.  Raises an exception if not found.
      function Get (Self : in Manager; Name : in Value)
                    return Value;

      procedure Insert (Self : in out Manager; Name : in Value;
                        Item : in Value);

      --  Set the value of the property.  The property is created if it
      --  does not exists.
      procedure Set (Self : in out Manager; Name : in Value;
                     Item : in Value);

      --  Remove the property given its name.
      procedure Remove (Self : in out Manager; Name : in Value);

      procedure Load_Properties (Self : in out Manager;
                                 File : in String);

      --  Deep copy of properties stored in 'From' to 'To'.
      function Create_Copy (Self : in Manager)
                           return Util.Properties.Interface_P.Manager_Access;

      procedure Delete (Self : in Manager;
                        Obj : in out Util.Properties.Interface_P.Manager_Access);

      function Get_Names (Self   : in Manager;
                          Prefix : in String) return Name_Array;

      procedure Add_Bundle (Self : in out Manager;
                            Props : in Util.Properties.Manager_Access);
   private
      use type Util.Properties.Manager_Access;

      package PropertyList is new Ada.Containers.Vectors
        (Element_Type => Util.Properties.Manager_Access,
         Index_Type => Natural,
           "=" => "=");

      type Manager is new Util.Properties.Interface_P.Manager with record
         List  : PropertyList.Vector;
         Props : aliased Util.Properties.Manager;
      end record;

      procedure Free is
        new Ada.Unchecked_Deallocation (Manager,
                                        Manager_Object_Access);
   end Interface_P;


   procedure Add_Bundle (Self  : in out Manager;
                         Props : in Manager_Access) is
      use type Util.Properties.Interface_P.Manager_Access;
   begin
      Interface_P.Manager'Class (Self.Impl.all).Add_Bundle (Props);
   end Add_Bundle;

   procedure Initialize (Object : in out Manager) is
      use Util.Properties.Interface_P;
   begin
      Object.Impl := new Util.Properties.Bundles.Interface_P.Manager;
      Object.Impl.Count := 1;
   end Initialize;

   procedure Adjust (Object : in out Manager) is
      use Util.Properties.Interface_P;
   begin
      if Object.Impl /= null then
         Object.Impl.Count := Object.Impl.Count + 1;
      else
         Object.Impl := new Util.Properties.Bundles.Interface_P.Manager;
         Object.Impl.Count := 1;
      end if;
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
      use type Util.Properties.Manager_Access;

      Loc_Name : constant String := '_' & Locale;
      Last_Pos : Integer := Loc_Name'Last;
   begin
      Log.Info ("Looking for bundle {0} and language {1}", Name, Locale);

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
                  Bundle.Impl.Count := Bundle.Impl.Count + 1;
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

      Path    : constant String := To_String (Factory.Path);
      Filter  : constant Filter_Type := (Ordinary_File => True, others => False);
      Pattern : constant String := Name & "*.properties";
      Ent     : Directory_Entry_Type;
      Names   : Util.Strings.String_Set.Set;
      Search  : Search_Type;
   begin
      Log.Info ("Reading bundle {1} in directory {0}", Path, Name);

      Found := False;
      Start_Search (Search, Directory => Path,
                    Pattern => Pattern, Filter => Filter);

      Factory.Lock.Write;

      begin
         --  Scan the directory and load all the property files.
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Simple      : constant String := Simple_Name (Ent);
               File_Path   : constant String := Full_Name (Ent);
               Bundle      : constant Bundle_Manager_Access := new Manager;
               Bundle_Name : constant Name_Access
                 := new String '(Simple (Simple'First .. Simple'Last - 11));
            begin
               Log.Debug ("Loading file {0}", File_Path);

               Interface_P.Manager'Class (Bundle.Impl.all).Load_Properties (File_Path);
               Factory.Bundles.Include (Key => Bundle_Name, New_Item => Bundle);
               Found := True;
               Names.Insert (Bundle_Name);
            end;
         end loop;

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

      --  ------------------------------
      --  Returns TRUE if the property exists.
      --  ------------------------------
      function Exists (Self : in Manager; Name : in Value)
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

      --  ------------------------------
      --  Returns the property value.  Raises an exception if not found.
      --  ------------------------------
      function Get (Self : in Manager; Name : in Value)
                   return Value is
      begin
         return Self.Props.Get (Name);

      exception
         when NO_PROPERTY =>
            declare
               Iter : Cursor := Self.List.First;
            begin
               while Has_Element (Iter) loop
                  begin
                     return Element (Iter).all.Get (Name);
                  exception
                     when NO_PROPERTY =>
                        Iter := Next (Iter);
                  end;
               end loop;
            end;
            raise;
      end Get;

      procedure Load_Properties (Self : in out Manager;
                                 File : in String) is
      begin
         Self.Props.Load_Properties (File);
      end Load_Properties;

      procedure Insert (Self : in out Manager;
                        Name : in Value;
                        Item : in Value) is
         pragma Unreferenced (Self);
         pragma Unreferenced (Name);
         pragma Unreferenced (Item);
      begin
         raise NOT_WRITEABLE with "Bundle is readonly";
      end Insert;

      --  ------------------------------
      --  Set the value of the property.  The property is created if it
      --  does not exists.
      --  ------------------------------
      procedure Set (Self : in out Manager;
                     Name : in Value;
                     Item : in Value) is
      begin
         raise NOT_WRITEABLE with "Bundle is readonly";
      end Set;

      --  ------------------------------
      --  Remove the property given its name.
      --  ------------------------------
      procedure Remove (Self : in out Manager; Name : in Value) is
      begin
         raise NOT_WRITEABLE with "Bundle is readonly";
      end Remove;

      --  ------------------------------
      --  Deep copy of properties stored in 'From' to 'To'.
      --  ------------------------------
      function Create_Copy (Self : in Manager)
                            return Util.Properties.Interface_P.Manager_Access is
         pragma Unreferenced (Self);
      begin
         return null;
      end Create_Copy;

      procedure Delete (Self : in Manager;
                        Obj : in out Util.Properties.Interface_P.Manager_Access) is
         pragma Unreferenced (Self);
         Item : Manager_Object_Access := Manager (Obj.all)'Access;
      begin
         Free (Item);
      end Delete;

      function Get_Names (Self   : in Manager;
                          Prefix : in String) return Name_Array is
         Result : Name_Array (1 .. 2);
         Iter   : constant Cursor := Self.List.First;
      begin
         while Has_Element (Iter) loop
            declare
               M : constant Util.Properties.Manager_Access := Element (Iter);
               N : constant Name_Array := M.Get_Names (Prefix);
            begin
               return N;
            end;
         end loop;
         return Result;
      end Get_Names;

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
   procedure Finalize (Factory : in out Loader) is
   begin
      Clear_Cache (Factory);
   end Finalize;

end Util.Properties.Bundles;
