-module(snake).
-export([sender/2, receiver/3]).


sender(ProcessNumber, ProcessLimit) ->
    random:seed(now()),
    Neighbor = integer_to_list(random:uniform(ProcessLimit)),
    NeighborSender = list_to_atom(string:concat("snake_sender_" , Neighbor)),
    NeighborReceiver = list_to_atom(string:concat("snake_receiver_" , Neighbor)),
    Receiver = list_to_atom(string:concat("snake_receiver_" , integer_to_list(ProcessNumber))),
    timer:sleep(1000),

    case (get(protocol) == undefined) of
        true ->
            receive
                {protocol, Protocol} -> put(protocol, Protocol)
            end;
        false -> do_nothing
    end,

    Receiver ! {get(protocol), senderping},
    receive
        {value, Value} -> thanks_receiver
    end,

    NeighborSender ! {protocol, get(protocol)},
    NeighborReceiver ! {ProcessNumber, get(protocol), Value},
    sender(ProcessNumber, ProcessLimit).


receiver(ProcessNumber, ProcessLimit, Fragment) ->
    Sender = list_to_atom(string:concat("snake_sender_" , integer_to_list(ProcessNumber))),

    case (get(longest) == undefined) of
        true ->
            compile:file(longest, [debug_info, export_all]),
            put(longest, longest:find_longest(Fragment));
        false ->
            do_nothing
    end,

    receive
        {1, senderping} ->
            Sender ! {value, get(longest)};
        {NeighborNumber, 1, LongestWord} ->
            case (length(LongestWord) >= length(get(longest))) of
                true ->
                    case (length(LongestWord) == length(get(longest))) of
                        true ->
                            io:format("Updated Word at Node ~w: ~s -> ~s~n~n", [ProcessNumber, get(longest), max(LongestWord, get(longest))]),
                            put(longest, max(LongestWord, get(longest)));
                        false ->
                            io:format("Updated Word at Node ~w: ~s -> ~s~n~n", [ProcessNumber, get(longest), LongestWord]),
                            put(longest, LongestWord)
                    end;
                false -> do_nothing
            end
    end,
    receiver(ProcessNumber, ProcessLimit, Fragment).
