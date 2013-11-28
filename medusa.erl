-module(medusa).
-export([assign_task/0, sender/1]).


assign_task() ->
    {ok, Socket} = gen_udp:open(8888, [binary, {active, false}]),
    Pid = spawn(medusa, sender, [Socket]),
    receiver(Socket).


sender(Socket) ->
    gen_udp:send(Socket, {127,0,0,1}, 8788, "Hey you good terminal, Happy Thanksgiving!").


receiver(Socket) ->
    X = gen_udp:recv(Socket, 0),
    case X of
        {ok, {_, _, Packet}} ->
            io:format("MESSAGE: ~s~n", [Packet]);
        {error, Reason} ->
            io:format("ERROR: ~s~n", [Reason])
    end,
    receiver(Socket).
