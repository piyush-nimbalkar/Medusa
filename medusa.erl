-module(medusa).
-export([assign_task/0]).


assign_task() ->
    {ok, Socket} = gen_udp:open(8888, [binary, {active, false}]),
    call_recv(Socket).


call_recv(Socket) ->
    X = gen_udp:recv(Socket, 0),
    io:format("---> ~p~n", [X]),
    call_recv(Socket).
