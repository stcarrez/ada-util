with Util.Processes;
procedure Env is
   P        : Util.Processes.Process;
begin
   Util.Processes.Set_Default_Environment (P);
   Util.Processes.Set_Environment (P, "ENV_VAR", "test1");
   Util.Processes.Spawn (P, "env");
   Util.Processes.Wait (P);
end Env;