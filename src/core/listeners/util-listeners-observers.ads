-----------------------------------------------------------------------
--  util-listeners-observers -- Observers design pattern
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--
--  The `Observers` package defines a simple listener interface with a single
--  `Update` operation that is called with an item as parameter.
generic
   type Element_Type (<>) is limited private;
package Util.Listeners.Observers is

   --  ------------------------------
   --  Observer
   --  ------------------------------
   type Observer is limited interface and Util.Listeners.Listener;

   --  The `Update` procedure is called by `Notify` with the given item.
   procedure Update (Instance : in Observer;
                     Item     : in Element_Type) is abstract;

   --  Notify the item passed in `Item` to the list of observers that have been registered
   --  in the `List`.
   procedure Notify (List : in Util.Listeners.List;
                     Item : in Element_Type);

end Util.Listeners.Observers;
