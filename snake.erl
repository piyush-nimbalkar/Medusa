-module(snake).
-export([sender/2, receiver/2]).


sender(ProcessNumber, ProcessLimit) ->
    random:seed(now()),
    Neighbor = list_to_atom(string:concat("snake_receiver_" , integer_to_list(random:uniform(ProcessLimit)))),
    Receiver = list_to_atom(string:concat("snake_receiver_" , integer_to_list(ProcessNumber))),
    timer:sleep(2000),
    Receiver ! {senderping},
    receive
        value -> Value = value
    end,
    Neighbor ! {ProcessNumber, ping},
    sender(ProcessNumber, ProcessLimit).


receiver(ProcessNumber, ProcessLimit) ->
    Sender = list_to_atom(string:concat("snake_sender_" , integer_to_list(ProcessNumber))),
    receive
        {senderping} -> Sender ! value;
        {NeighborNumber, _} ->
            io:format("Received Ping from Node ~w to Node ~w~n", [NeighborNumber, ProcessNumber])
    end,
    receiver(ProcessNumber, ProcessLimit).
