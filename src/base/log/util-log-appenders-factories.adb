-----------------------------------------------------------------------
--  util-log-appenders -- Log appenders
--  Copyright (C) 2001 - 2019, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body Util.Log.Appenders.Factories is

   Factory : aliased Appender_Factory
        := Appender_Factory '(Length  => Name'Length,
                              Name    => Name,
                              Factory => Create,
                              Next_Factory => Appender_Factories);

begin
   Appender_Factories := Factory'Access;
end Util.Log.Appenders.Factories;
