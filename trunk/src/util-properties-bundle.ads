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

package Util.Properties.Bundle is

   type Manager is new Util.Properties.Manager with private;

   procedure Add_Bundle (Self : in out Manager; Props : in Manager_Access);
   --  Add a bundle

   procedure Load_Bundle (Self : in out Manager;
                          Path : in String;
                          Name : in String);

   procedure Find_Bundle (Self   : in Manager;
                          Locale : in String;
                          Bundle  : out Manager);

private

   type Manager is new Util.Properties.Manager with record
      A : Integer := 0;
   end record;

   procedure Initialize (Object : in out Manager);
   procedure Adjust (Object : in out Manager);

end Util.Properties.Bundle;
