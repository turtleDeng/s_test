-module (s_test).
-export ([start/0]).

start() ->
	crypto:start(),
    application:start(emysql),
	application:start(s_test).