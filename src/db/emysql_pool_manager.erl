%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.local>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2012 by Chen Slepher <slepher@larry.local>
%%%-------------------------------------------------------------------
-module(emysql_pool_manager).

-behaviour(gen_server).

%% API
-export([pool/1]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {pools}).

%%%===================================================================
%%% API
%%%===================================================================
pool(PoolId) ->
    gen_util:call(?SERVER, {pool, PoolId}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(MysqlOpts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MysqlOpts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([MysqlOpts]) ->
    io:format("start_pools-----------~n"),
    StartedPools =  case MysqlOpts of
                        undefined -> [];
                        MysqlOpts when is_list(MysqlOpts) ->
                            start_pools(MysqlOpts);
                        MysqlOpts when is_tuple(MysqlOpts) ->
                            lists:foreach(fun(Opt) ->
                                                  start_pools(Opt)
                                          end, tuple_to_list(MysqlOpts))
                    end,
    {ok, #state{pools = StartedPools}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({pool, PoolId}, _From, #state{pools = Pools} = State) ->
    Reply = 
        case lists:member(PoolId, Pools) of
            true ->
                {ok, ok};
            false ->
                {error, pool_not_exits}
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_pools(MysqlOpt) when is_list(MysqlOpt) ->
    Host = proplists:get_value(host, MysqlOpt),
    Port = proplists:get_value(port, MysqlOpt),
    UserName = proplists:get_value(username, MysqlOpt),
    Password = proplists:get_value(password, MysqlOpt),
    Database = proplists:get_value(database, MysqlOpt),
    Encoding = proplists:get_value(encoding, MysqlOpt),
    Pools = proplists:get_value(pools, MysqlOpt),
    lists:foldl(
      fun({PoolName, PoolSize}, AccIn) ->
              case emysql:add_pool(PoolName, PoolSize, UserName, Password,
                                   Host, Port, Database, Encoding) of
                  ok -> [PoolName | AccIn];
                  {error, Reason} ->
                      io:format("EMysqlPool add pool<~p> failed: ~p",
                                 [PoolName, Reason])
              end
      end, [], Pools).
