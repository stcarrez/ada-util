-----------------------------------------------------------------------
--  util-concurrent-copies -- Concurrent Tools
--  Copyright (C) 2011, 2017 Stephane Carrez
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

package body Util.Concurrent.Copies is

   protected body Atomic is
      --  ------------------------------
      --  Get the value atomically
      --  ------------------------------
      function Get return Element_Type is
      begin
         return Value;
      end Get;

      --  ------------------------------
      --  Change the value atomically.
      --  ------------------------------
      procedure Set (Object : in Element_Type) is
      begin
         Value := Object;
      end Set;

   end Atomic;

end Util.Concurrent.Copies;
