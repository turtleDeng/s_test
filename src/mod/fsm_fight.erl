-module (fsm_fight).
-behavior(gen_fsm).

-export([start_link/0, use_skill/1]).
-export([init_fight/2, fight_wait/2, fight/2, over/2, init/1, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


-define(WAIT_FIGHT_TIME,  10000).   

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).


use_skill(Pid) ->
    gen_fsm:send_event(Pid, {skill, 1, 10}).    


-record (state, {aplayer_list = [], bplayer_list = [], time}).

-record (attr, {battle,
                hp,
                time}).


init([]) ->
    process_flag(trap_exit, true),
    {ok, init_fight, #state{}}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
 
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
 
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
 
terminate(_Reason, _StateName, _State) ->
    ok.
 
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

init_fight({init, APLayerList, BPLayerList}, State) ->
    APlayerAtt = lists:map(
                    fun(Player) ->
                        {Player, #attr{battle    = get_a_battle(),
                                       hp        = get_init_hp(),
                                       time      = ?WAIT_FIGHT_TIME}}
                    end, APLayerList),

    BPlayerAtt = lists:map(
                    fun(Player) ->
                        {Player, #attr{battle    = get_b_battle(),
                                       hp        = get_init_hp(),
                                       time      = ?WAIT_FIGHT_TIME}}
                    end, BPLayerList),

    NState = State#state{aplayer_list = APlayerAtt, 
                         bplayer_list = BPlayerAtt,
                         time         = get_time()},                         
	{next_state, fight_wait, NState, ?WAIT_FIGHT_TIME}.

% fight_wait({use_goods, Dir , PlayerId, Battle2}, State) ->
%     {next_state, fight_wait, State};

% fight_wait({use_, Dir , PlayerId, Battle2}, State) ->
%     {next_state, fight_wait, State};    

fight_wait({use_skill, Dir , PlayerId, Battle2}, State) ->
    APlayerList = State#state.aplayer_list,
    BPlayerList = State#state.bplayer_list,
    {NewAPlayerList1, NewBPlayerList1} = 
        case Dir of
            a ->
                {Player, Attr} = lists:keyfind(PlayerId, 1, APlayerList),
                NewAPlayerList = lists:keyreplace(Player, 1, APlayerList, {Player, Attr#attr{battle = Attr#attr.battle + Battle2, time = 0}}),
                {NewAPlayerList, BPlayerList};
            b ->
                {Player, Attr} = lists:keyfind(PlayerId, 1, BPlayerList),
                NewBPlayerList = lists:keyreplace(Player, 1, BPlayerList, {Player, Attr#attr{battle = Attr#attr.battle + Battle2, time = 0}}),
                {APlayerList, NewBPlayerList}
        end,
    Time = get_time(),      
    case is_fight_wait(NewAPlayerList1 ++ NewBPlayerList1) of
        true ->
            {next_state, fight_wait, State#state{aplayer_list = NewAPlayerList1, 
                                            bplayer_list = NewBPlayerList1,
                                            time         = Time}, 
                                ?WAIT_FIGHT_TIME - ((Time - State#state.time) * 1000)};
        false ->
            {next_state, fight_wait, State#state{aplayer_list = NewAPlayerList1, 
                                            bplayer_list = NewBPlayerList1,
                                            time         = Time}, 0}
    end;

fight_wait(timeout, State) ->
    io:format("fight_wait.....~n"),
    {next_state, fight, State, 0}.

fight(timeout, State) ->
    APlayerList = State#state.aplayer_list,
    BPlayerList = State#state.bplayer_list,
    Fun = fun({APlayer, Attr}, {AccIn, AccIn2}) -> 
                        case get_b(AccIn2) of
                            over -> 
                                {AccIn, AccIn2};
                            {BPlayer, BAttr} ->
                            NewHp = BAttr#attr.hp - Attr#attr.battle,
                            {[{APlayer, BPlayer, NewHp} | AccIn], 
                             lists:keyreplace(BPlayer, 1, AccIn2, {BPlayer, BAttr#attr{hp = NewHp, time = ?WAIT_FIGHT_TIME}})}
                        end
                    end,
    {ABattleData, NewBPlayerList} = lists:foldl(Fun, {[], BPlayerList}, APlayerList),
    {BBattleData, NewAPlayerList} = lists:foldl(Fun, {[], APlayerList}, BPlayerList),

    io:format("ABattleData:~p~n", [ABattleData]),
    io:format("BBattleData:~p~n", [BBattleData]),
    io:format("APlayerList:~p~n", [NewAPlayerList]),
    io:format("BPlayerList:~p~n", [NewBPlayerList]),
    NState = State#state{aplayer_list = NewAPlayerList, bplayer_list = NewBPlayerList, time = get_time()},
    
    %%send_data_to_client(NewAPlayerList ++ NewBPlayerList, ABattleData ++ BBattleData),

    case (is_geme_over(NewAPlayerList)) and (is_geme_over(NewBPlayerList))of
        true -> 
            {next_state, fight_wait, NState, ?WAIT_FIGHT_TIME};
        false ->
            {next_state, over, NState, 0}
    end;

fight(Event, State) ->
    io:format("fight Event:~p~n", [Event]),
    {next_state, over, State}.

over(timeout, State) ->
    io:format("over timeout~n"),
	{stop, normal, State};

over(Event, State) ->
    io:format("figth Event:~p~n", [Event]),
    {stop, normal, State}.

get_init_hp() ->
    100.

get_a_battle() ->
    10.

get_b_battle() ->
    20.

get_start_time() ->
    30000.

get_time() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

get_random(Len) ->
    random:seed(now()),
    random:uniform(Len).

get_b(List) when length(List) > 0  ->
    {BPlayer, Attr} = lists:nth(get_random(length(List)), List),
    case Attr#attr.hp > 0 of
        true ->
            {BPlayer, Attr};             
        false ->
            List2 = lists:keydelete(BPlayer, 1, List),
            get_b(List2)
    end;
get_b(_) ->
    over.

is_geme_over(PlayerList) ->
    lists:any(fun({_, Attr}) -> Attr#attr.hp > 0 end, PlayerList).

send_data_to_client(PlayerList, Data) ->
    lists:foreach(
        fun({Player, _}) -> 
            case lib_player:get_player(Player) of        
                undefined -> io:format("ont find APlayer:~p pid~n", [Player]);
                BPlayerPid -> gen_server:cast(BPlayerPid, {fight_data, list_to_binary(data_to_binary(Data))})
            end
        end, PlayerList).

data_to_binary(Data) ->
    lists:map(
        fun({APlayer, BPlayer, Hp}) -> 
            <<APlayer:32, BPlayer:32, Hp:32>>
        end,  Data).            


is_fight_wait(PlayerList) ->
    lists:any(
        fun({_, Attr}) ->
            Attr#attr.time =/= 0 
        end,PlayerList).    