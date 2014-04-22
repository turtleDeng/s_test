-module (lib_team).

-export ([create/1,
          apply/2,
          join/2,
          remove/2,
          clear_apply_list/1,
          dismiss/1,
          show/1]).

-include ("define_team.hrl").
-include ("define_player.hrl").

%% 创建队伍　
create(Player) ->
    PlayerId = Player#player.player_id,
    Formation = 1,
    ets:insert(?TEAM_ETS, {PlayerId, {Formation, [PlayerId], []}}).

%% 申请加入队伍　
apply(Player, ApplyPlayer) ->
    PlayerId = Player#player.player_id,
    ApplyPLayerId = ApplyPlayer#player.player_id,
    case ets:lookup(?TEAM_ETS, PlayerId) of
        [{TeamId, {Formation, Players, ApplyLists}}] ->
            NewApplyList = 
                case length(Players) < 5 of
                    false ->
                        io:format("~ts", [<<"队伍已满">>]),
                        ApplyLists;
                    true ->
                        case lists:member(ApplyPLayerId, Players) of
                            true ->
                                io:format("~ts", [<<"你已经在队伍了">>]),
                                ApplyLists;
                            false ->
                                case lists:member(ApplyPLayerId, ApplyLists) of
                                    true ->
                                        io:format("~ts", [<<"你已经申请加入队伍,无需重复申请">>]),
                                        ApplyLists;
                                    false ->
                                        case is_team(ApplyPLayerId) of
                                            true ->
                                                io:format("~ts", [<<"你已经有队伍了">>]),
                                                ApplyLists;
                                            false ->
                                                [ApplyPLayerId| ApplyLists]        
                                        end
                                end                                    
                        end                        
                end,
            ets:insert(?TEAM_ETS, {TeamId, {Formation, Players, NewApplyList}}),
            notice_captain(TeamId, NewApplyList);
        [] -> error
    end.

%% 加入　
join(Player, JoinPlayer) ->
    PlayerId = Player#player.player_id,
    JoinPLayerId = JoinPlayer#player.player_id,
    case ets:lookup(?TEAM_ETS, PlayerId) of
        [{TeamId, {Formation, Players, ApplyLists}}] ->
            case (length(ApplyLists) =/= 0) and (lists:member(JoinPLayerId, ApplyLists)) of
                true ->
                    case length(Players) < 5 of
                        true ->
                            case is_team(JoinPLayerId) of
                                true ->
                                    io:format("~ts", [<<"对方已有队伍了">>]);
                                false ->
                                    NewPlayers = [JoinPLayerId | Players],
                                    NewApplyLists = ApplyLists -- [JoinPLayerId],
                                    ets:insert(?TEAM_ETS, {TeamId, {Formation, NewPlayers, NewApplyLists}}),
                                    notice_team_member(JoinPLayerId, <<"">>)
                            end;
                        false ->
                            io:format("~ts", [<<"队伍已满">>])
                    end;                        
                    
                false ->
                    io:format("~ts", [<<"error">>])
            end;
        [] -> error
    end.            

%% 踢出　
remove(Player, RemovePlayer) ->
    PlayerId = Player#player.player_id,
    RemovePlayerId = RemovePlayer#player.player_id,
    case ets:lookup(?TEAM_ETS, PlayerId) of
        [{TeamId, {Formation, Players, ApplyLists}}] ->
            case lists:member(RemovePlayerId, Players) of
                true -> 
                    NewPlayers = Players -- [RemovePlayerId],
                    ets:insert(?TEAM_ETS, {TeamId, {Formation, NewPlayers, ApplyLists}}),
                    notice_team_member(RemovePlayerId, <<"">>);
                false ->
                    io:format("~ts", [<<"error">>])
            end;
        [] -> error
    end.

%% 清空申请列表　
clear_apply_list(Player) ->

    case ets:lookup(?TEAM_ETS, Player#player.player_id) of
        [{TeamId, {Formation, Players, _ApplyLists}}] ->
            ets:insert(?TEAM_ETS, {TeamId, {Formation, Players, []}});
        [] -> error
    end.

%% 解散队伍
dismiss(Player) ->
    PlayerId = Player#player.player_id,
    case ets:lookup(?TEAM_ETS, PlayerId) of
        [{_TeamId, {_Formation, Players, _ApplyLists}}] ->
            ets:delete(?TEAM_ETS, PlayerId),
            notice_team_member(Players, <<"">>);
        [] -> error
    end.

show(Player) ->
    case ets:lookup(?TEAM_ETS, Player#player.player_id) of
        [{TeamId, {Formation, Players, ApplyLists}}] ->
            io:format("TeamId:~p~n", [TeamId]),
            io:format("Formation:~p~n", [Formation]),
            io:format("Players:~p~n", [Players]),
            io:format("ApplyLists:~p~n", [ApplyLists]);
        [] -> error
    end.             

is_team(PlayerId) ->
    TeamList = ets:match(team_ets, {'_', {'_', '$1', '_'}}),
    lists:member(PlayerId, lists:flatten(TeamList)).


notice_captain(PlayerId, NewPlayers) ->
    ok.

notice_team_member(Players, Msg) when is_list(Players) and Players =:= [] ->
    [Player | Players2] = Players,
    notice_team_member(Player, Msg),
    notice_team_member(Players2, Msg);

notice_team_member(Player, Msg) -> 
    ok.         