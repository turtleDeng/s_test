%%%-----------------------------------
%%% @Module  : tcp_listener
%%% @Created : 2011.06.01 
%%% @Description: tcp listerner监听
%%%-----------------------------------

-module(tcp_listener).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("define_network.hrl").
 
-record(state, {listener}).

start_link(AcceptorCount, Port)
  when is_integer(Port) ->
    %gen_server:start_link({local, ?MODULE}, ?MODULE, [AcceptorCount, Port], []).
    gen_server:start_link(?MODULE, [AcceptorCount, Port], []).
 
init([AcceptorCount, Port]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSock} ->
            io:format("listen port:~p~n", [Port]),
            lists:foreach(fun (_) ->
                            
                            {ok, _APid} = supervisor:start_child(tcp_acceptor_sup, [LSock])
                          end,
                          lists:seq(1, AcceptorCount)),
            {ok, #state{listener = LSock}};
        {error, Reason} ->
            {stop, #state{}}
    end.

handle_call(_Request, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(Info, State) ->
    {noreply, State}.
 
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.