-module(tcp_listener_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Port) ->
    supervisor:start_link(?MODULE, {10, Port}).

init({AcceptorCount, Port}) ->
    {ok,
        {{one_for_all, 10, 10},
            [
                {
                    tcp_acceptor_sup,
                    {tcp_acceptor_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [tcp_acceptor_sup]
                },
                {
                    listener_sup,
                    {listener_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [listener_sup]
                },
                {
                    lists:concat([tcp_listener_,Port]),
                    {tcp_listener, start_link, [AcceptorCount, Port]},
                    transient,
                    100,
                    worker,
                    [tcp_listener]
                },
				{
                    lists:concat([tcp_listener_,Port-100]),
                    {tcp_listener, start_link, [AcceptorCount, Port-100]},
                    transient,
                    100,
                    worker,
                    [tcp_listener]
                },
				{
                    lists:concat([tcp_listener_,Port-200]),
                    {tcp_listener, start_link, [AcceptorCount, Port-200]},
                    transient,
                    100,
                    worker,
                    [tcp_listener]
                }
            ]
        }
    }.
