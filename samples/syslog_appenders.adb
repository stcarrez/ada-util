with System;
with Interfaces.C;
with Ada.Finalization;
with Util.Systems.Constants;
package body Syslog_Appenders is

   use Util.Systems.Constants;
   use type System.Address;

   type Syslog_Appender_Access is access all Syslog_Appender'Class;

   procedure Sys_Openlog (Ident    : System.Address;
                          Options  : Interfaces.C.int;
                          Facility : Interfaces.C.int)
     with Import => True, Convention => C,
     Link_Name => SYMBOL_PREFIX & "openlog";

   procedure Sys_Syslog (Priority : Interfaces.C.int;
                         Format   : System.Address;
                         Param1   : System.Address)
     with Import => True, Convention => C,
     Link_Name => SYMBOL_PREFIX & "syslog";

   --  Use weak symbols to avoid compilation issues when this is not available.
   pragma Weak_External (Sys_Openlog);
   pragma Weak_External (Sys_Syslog);

   LOG_PID    : constant Interfaces.C.int := 1;
   LOG_ERROR  : constant Interfaces.C.int := 3;
   LOG_LOCAL1 : constant Interfaces.C.int := 136;

   function Create (Name       : in String;
                    Properties : in Util.Properties.Manager;
                    Default    : in Util.Log.Level_Type)
                    return Util.Log.Appenders.Appender_Access is
      Result : constant Syslog_Appender_Access
        := new Syslog_Appender '(Ada.Finalization.Limited_Controlled
                                   with Length => Name'Length,
                                 Name => Name,
                                 others => <>);
   begin
      Result.Set_Level (Name, Properties, Default);
      Result.Set_Layout (Name, Properties, Util.Log.Appenders.FULL);
      if Sys_Openlog'Address /= System.Null_Address then
         Sys_Openlog (Result.Name'Address, LOG_PID, LOG_LOCAL1);
      end if;
      return Result.all'Access;
   end Create;

   overriding
   procedure Append (Self    : in out Syslog_Appender;
                     Message : in Util.Strings.Builders.Builder;
                     Date    : in Ada.Calendar.Time;
                     Level   : in Util.Log.Level_Type;
                     Logger  : in String) is
      pragma Unreferenced (Date, Logger);

      Def_Format : aliased constant String := "%s" & ASCII.NUL;
   begin
      if Self.Level >= Level and then Sys_Syslog'Address /= System.Null_Address
      then
         declare
            Msg : constant String
              := Util.Strings.Builders.To_Array (Message) & ASCII.NUL;
         begin
            Sys_Syslog (LOG_ERROR, Def_Format'Address, Msg'Address);
         end;
      end if;
   end Append;

end Syslog_Appenders;
