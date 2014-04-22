-module (mod_player).


-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
-export([start/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("define_network.hrl").
-include("define_player.hrl").
-record(state, {player_id, client_pid}).

 start(PlayerId, #client{}=Client) ->
    Name = lib_player:get_player_name(PlayerId),
    gen_server:start_link( {local,Name}, ?MODULE, [PlayerId, Client], []).

init([PlayerId, Client]) ->
    try
        init_player_data(Client, PlayerId)
    catch
        _:Error -> 
            io:format("Cmd:~p, error:~p~n, ~p~n",  [1001, Error, erlang:get_stacktrace()])
    end,
    {ok, #state{player_id = PlayerId,
                client_pid = Client#client.client_pid}}.

handle_call(test, From, State) ->
    io:format("first player"),
    send(State#state.client_pid, <<"first player">>),
    Reply = ok,
    {reply, Reply, State};
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({10001, PlayerId2}, State) ->
    io:format("10001 Msg:~p~n", [PlayerId2]),
    case lib_player:get_player(PlayerId2) of
        undefined ->    send(State#state.client_pid, 10000000);
        _ ->
            APLayerList = lib_player:get_team(a),
            BPLayerList = lib_player:get_team(b),
            {ok, Pid} = fsm_fight:start_link(),
            % gen_fsm:send_event(Pid, {skill, a , 10001, 10}).
            % gen_fsm:send_event(Pid, {skill, b , 10006, 5}).
            gen_fsm:send_event(Pid, {init, APLayerList, BPLayerList})
    end,        
    {noreply, State};



handle_cast({fight_data, Data}, State) ->
    io:format("fight_data:~p~n", [Data]),
    send(State#state.client_pid, Data), 
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("Msg:~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Info:~p~n", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    io:format("Reason:~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send(ClientPid, Data) ->
    gen_server:cast(ClientPid, {cmd_send, Data}).

init_player_data(Client, PlayerId) ->
    {I, I2, I3, I4} = Client#client.addr,
    mysql_utils:save(pool, player , #player{username  = integer_to_list(PlayerId) ++ "name",
                                            last_time = get_time(),
                                            last_ip   = lists:concat([I,":", I2, ":",I3, ":",I4])}, 
                                    record_info(fields, player)).

get_time() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.