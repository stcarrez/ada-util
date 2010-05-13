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

with Ada.Containers;
with Ada.Containers.Vectors;
package body Util.Properties.Bundle is

   --  Implementation of the Bundle
   --  (this allows to decouples the implementation from the API)
   package Interface_P is

      type Manager is new Util.Properties.Interface_P.Manager with private;

      function Exists (Self : in Manager; Name : in Value)
                       return Boolean;
      --  Returns TRUE if the property exists.

      function Get (Self : in Manager; Name : in Value)
                    return Value;
      --  Returns the property value.  Raises an exception if not found.

      procedure Insert (Self : in out Manager; Name : in Value;
                        Item : in Value);

      procedure Set (Self : in out Manager; Name : in Value;
                     Item : in Value);
      --  Set the value of the property.  The property is created if it
      --  does not exists.

      procedure Remove (Self : in out Manager; Name : in Value);
      --  Remove the property given its name.

      function Create_Copy (Self : in Manager)
                           return Util.Properties.Interface_P.Manager_Access;
      --  Deep copy of properties stored in 'From' to 'To'.

      procedure Delete (Self : in Manager;
                        Obj : in out Util.Properties.Interface_P.Manager_Access);

      function Get_Names (Self : in Manager) return Name_Array;

      procedure Add_Bundle (Self : in out Manager;
                            Props : in Util.Properties.Manager_Access);
   private
      use type Util.Properties.Manager_Access;

      package PropertyList is new Ada.Containers.Vectors
        (Element_Type => Util.Properties.Manager_Access,
         Index_Type => Natural,
           "=" => "=");

      type Manager is new Util.Properties.Interface_P.Manager with record
         List : PropertyList.Vector;
      end record;
   end Interface_P;


   procedure Add_Bundle (Self  : in out Manager;
                         Props : in Manager_Access) is
   begin
      Interface_P.Manager'Class (Self.Impl.all).Add_Bundle (Props);
   end Add_Bundle;

   procedure Load_Bundle (Self : in out Manager;
                          Path : in String;
			  Name : in String) is
   begin
      null;
   end Load_Bundle;

   procedure Find_Bundle (Self   : in Manager;
			  Locale : in String;
			  Bundle  : out Manager) is
   begin
      Bundle := Self;
   end Find_Bundle;

   procedure Initialize (Object : in out Manager) is
      use Util.Properties.Interface_P;
   begin
      Object.Impl := new Util.Properties.Bundle.Interface_P.Manager;
      Object.Impl.Count := 1;
   end Initialize;

   procedure Adjust (Object : in out Manager) is
      use Util.Properties.Interface_P;
   begin
      if Object.Impl /= null then
         Object.Impl.Count := Object.Impl.Count + 1;
      else
         Object.Impl := new Util.Properties.Bundle.Interface_P.Manager;
         Object.Impl.Count := 1;
      end if;
   end Adjust;

   --  Implementation of the Bundle
   --  (this allows to decouples the implementation from the API)
   package body Interface_P is
      use PropertyList;

      --  Returns TRUE if the property exists.
      function Exists (Self : in Manager; Name : in Value)
                      return Boolean is
         Iter : Cursor := Self.List.First;
      begin
         while Has_Element (Iter) loop
            if Element (Iter).Exists (Name) then
               return True;
            end if;
            Iter := Next (Iter);
         end loop;
         return False;
      end Exists;

      --  Returns the property value.  Raises an exception if not found.
      function Get (Self : in Manager; Name : in Value)
                   return Value is
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
         raise NO_PROPERTY;
      end Get;

      procedure Insert (Self : in out Manager;
                        Name : in Value;
                        Item : in Value) is
         Props : Util.Properties.Manager_Access;
      begin
         if Self.List.Is_Empty then
            Props := new Util.Properties.Manager;
            Self.List.Append (Props);
         else
            Props := Self.List.First_Element;
         end if;
         Props.Set (Name, Item);
      end Insert;

      --  Set the value of the property.  The property is created if it
      --  does not exists.
      procedure Set (Self : in out Manager;
                     Name : in Value;
                     Item : in Value) is
         Props : Util.Properties.Manager_Access;
      begin
         if Self.List.Is_Empty then
            Props := new Util.Properties.Manager;
            Self.List.Append (Props);
         else
            Props := Self.List.First_Element;
         end if;
         Props.Set (Name, Item);
      end Set;

      --  Remove the property given its name.
      procedure Remove (Self : in out Manager; Name : in Value) is
         Iter : Cursor := Self.List.First;
      begin
         while Has_Element (Iter) loop
            begin
               Element (Iter).Remove (Name);
            exception
               when NO_PROPERTY =>
                  null;
            end;
            Iter := Next (Iter);
         end loop;
      end Remove;

      --  Deep copy of properties stored in 'From' to 'To'.
      function Create_Copy (Self : in Manager)
                           return Util.Properties.Interface_P.Manager_Access is
      begin
         return null;
      end Create_Copy;

      procedure Delete (Self : in Manager;
                        Obj : in out Util.Properties.Interface_P.Manager_Access) is
      begin
         null;
      end Delete;

      function Get_Names (Self : in Manager) return Name_Array is
         Result : Name_Array (1 .. 2);
      begin
         return Result;
      end Get_Names;

      procedure Add_Bundle (Self  : in out Manager;
                            Props : in Util.Properties.Manager_Access) is
      begin
         Self.List.Append (Props);
      end Add_Bundle;
   end Interface_P;

end Util.Properties.Bundle;
