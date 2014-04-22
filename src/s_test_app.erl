-module(s_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, MySql} = application:get_env(mysql_opt), 
	io:format("mysql:~p~n", [MySql]),
    mod_kernel:start(),
    case tcp_listener_sup:start_link(8484) of
        {ok, _Pid} ->
            io:format("listener_sup started for port: ~p ... ~n", [8484]);
        Other ->
            io:format("listener_sup started for port failed, reason: ~p~n",
                       [Other])
    end,
    s_test_sup:start_link(MySql).

stop(_State) ->
    ok.
