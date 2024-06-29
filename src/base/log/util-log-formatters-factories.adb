-----------------------------------------------------------------------
--  util-log-formatters-factories -- Factory for log formatters
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body Util.Log.Formatters.Factories is

   Factory : aliased Formatter_Factory
        := Formatter_Factory '(Length  => Name'Length,
                               Name    => Name,
                               Factory => Create,
                               Next_Factory => Formatter_Factories);

begin
   Formatter_Factories := Factory'Access;
end Util.Log.Formatters.Factories;
