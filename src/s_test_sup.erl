
-module(s_test_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(MySql) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MySql]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([MySql]) ->
    {ok, {{one_for_one, 5, 10}, 
          [{emysql_pool_manager,
           {emysql_pool_manager, start_link, [MySql]},
            transient, 2000, worker, [emysql_pool_manager]}]} }.

