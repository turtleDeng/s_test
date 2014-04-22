-include_lib("stdlib/include/ms_transform.hrl").

%% ---------------------------------
%% Logging mechanism
%% Print in standard output
-define(TAG_PRINT(Tag, Format, Args),
        io:format("(~p:~s:~p:~p:~p) : " ++ Format, 
                  [Tag, misc:time_format(now()), self(), ?MODULE, ?LINE] ++ Args)).
-define(PRINT(Format, Args),
        io:format("(~s:~p:~p:~p) : " ++ Format, 
                  [misc:time_format(now()), self(), ?MODULE, ?LINE] ++ Args)).
-define(TEST_MSG(Format, Args),
        logger:test_msg(?MODULE, ?LINE, Format, Args)).
-define(DEBUG(Format, Args),
        logger:debug_msg(?MODULE, ?LINE, Format, Args)).
-define(INFO_MSG(Format, Args),
        logger:info_msg(?MODULE, ?LINE, Format, Args)).
-define(WARNING_MSG(Format, Args),
        logger:warning_msg(?MODULE, ?LINE, Format, Args)).
-define(ERROR_MSG(Format, Args),
        logger:error_msg(?MODULE, ?LINE, Format, Args)).
-define(CRITICAL_MSG(Format, Args),
        logger:critical_msg(?MODULE, ?LINE, Format, Args)).

%% 为了测试方便用的宏。
-define(H,
        logger:error_msg(?MODULE, ?LINE, "###### only trace!~n",   [])).
-define(H(Arg),
        logger:error_msg(?MODULE, ?LINE, "###### only trace:~w~n", [Arg])).