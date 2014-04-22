-module (mysql_test).
-export([run/0]).
-include("define_player.hrl").
-record (hello_table, {hello_text = 0}).
run() ->

	%%mysql_utils:save(pool, hello_table, #hello_table{hello_text = "123"}, record_info(fields, hello_table)),
    %%{ _, _, _, Result, _ } = emysql:execute(pool, <<"select hello_text from hello_table">>),

	mysql_utils:save(pool, player , #player{username  = "123",
                                            last_time = 1000,
                                            last_ip   = "123"}, 
                                    record_info(fields, player)).
    % Result = mysql_utils:find(pool, hello_table, first, [], hello_table, record_info(fields, hello_table)),
    % io:format("~n~p~n", [Result#hello_table.hello_text]).

	%emysql:add_pool(pool, 1,"root", "123456", "192.168.87.177", 3306, "game", utf8).