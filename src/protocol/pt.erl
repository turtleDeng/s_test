-module(pt).
-export([
         routing/1
        ]).


%% 路由信息，根据Cmd获取protobuf处理module和回调module
routing(Cmd) ->
    case Cmd div 1000 of
        10 -> pp_account;
        _ ->
            error
    end.