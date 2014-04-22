
-module(listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([init/1]).

-include("define_network.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{tcp_client_handler, {tcp_client_handler, start_link, []},
            transient, brutal_kill, worker, [tcp_client_handler]}]}}.  