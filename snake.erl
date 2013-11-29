-module(medusa).
-export([start/0, sender/1]).


start() ->
    {ok, Socket} = gen_udp:open(8888, [binary, {active, false}]),
    SenderPid = spawn(medusa, sender, [Socket]),
    receiver(Socket, SenderPid).


sender(Socket) ->
    gen_udp:send(Socket, {127,0,0,1}, 8788, "Hey you good terminal, Happy Thanksgiving!"),
    receive
        ping ->
            sender(Socket)
    end.


receiver(Socket, SenderPid) ->
    X = gen_udp:recv(Socket, 0),
    case X of
        {ok, {_, _, Packet}} ->
            io:format("MESSAGE: ~s~n", [Packet]);
        {error, Reason} ->
            io:format("ERROR: ~s~n", [Reason])
    end,
    SenderPid ! ping,
    receiver(Socket, SenderPid).
