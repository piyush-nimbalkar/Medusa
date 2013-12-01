-module(snake).
-export([sender/2, receiver/3]).


sender(ProcessNumber, ProcessLimit) ->
    random:seed(now()),
    Neighbor = list_to_atom(string:concat("snake_receiver_" , integer_to_list(random:uniform(ProcessLimit)))),
    Receiver = list_to_atom(string:concat("snake_receiver_" , integer_to_list(ProcessNumber))),
    timer:sleep(2000),

    Receiver ! {senderping},
    receive
        Value -> awesome
    end,

    Neighbor ! {ProcessNumber, Value},
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
        {senderping} -> Sender ! get(longest);
        {NeighborNumber, LongestWord} ->
            case (length(LongestWord) >= length(get(longest))) of
                true ->
                    case (length(LongestWord) == length(get(longest))) of
                        true -> put(longest, max(LongestWord, get(longest)));
                        false -> put(longest, LongestWord)
                    end;
                false -> do_nothing
            end,
            io:format("Updated Word at Node ~w: ~s~n~n", [ProcessNumber, get(longest)])
    end,
    receiver(ProcessNumber, ProcessLimit, Fragment).
