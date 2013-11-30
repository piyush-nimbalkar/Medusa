-module(snake).
-export([start/1, sender/2]).


start(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
    %% SenderPid = spawn(medusa, sender, [Socket, 8788]),
    %% receiver(Socket, SenderPid).
    %% sender(Socket, 8788),
    sender(Socket, 8788).


sender(Socket, Port) ->
    gen_udp:send(Socket, {127,0,0,1}, Port, "Hey you good terminal, Happy Thanksgiving!").
    %% receive
        %% ping ->
            %% sender(Socket)
    %% end.


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
