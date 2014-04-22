-module (mod_kernel).

-export ([start/0]).

start() ->
	{ok, _Pid} = mod_team:start_link().