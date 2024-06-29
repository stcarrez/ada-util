-----------------------------------------------------------------------
--  Util.Beans.Objects.Hash -- Hash on an object
--  Copyright (C) 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Containers;
function Util.Beans.Objects.Hash (Key : in Object) return Ada.Containers.Hash_Type;
pragma Preelaborate (Util.Beans.Objects.Hash);
