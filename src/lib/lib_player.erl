-module (lib_player).
-export ([get_player/1, get_player_name/1, get_team/1]).

get_player(PlayerId) ->
	Name = get_player_name(PlayerId),
	erlang:whereis(Name).


get_player_name(PlayerId) ->
	list_to_atom(integer_to_list(PlayerId) ++ "mod_player").


get_team(a) ->
	[10001];
	%[10001, 10002, 10003, 10004, 10005];


get_team(b) ->
	[10006].
	%[10006, 10007, 10008, 10009, 10010].