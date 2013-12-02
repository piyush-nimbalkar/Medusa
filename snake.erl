-module(snake).
-export([sender/2, start_receiver/3]).


start_receiver(ProcessNumber, ProcessLimit, Fragment) ->
    put(process_number, ProcessNumber),
    put(fragment, Fragment),
    receiver().


sender(ProcessNumber, ProcessLimit) ->
    random:seed(now()),
    Neighbor = integer_to_list(random:uniform(ProcessLimit)),
    NeighborSender = list_to_atom(string:concat("snake_sender_" , Neighbor)),
    NeighborReceiver = list_to_atom(string:concat("snake_receiver_" , Neighbor)),
    OwnReceiver = list_to_atom(string:concat("snake_receiver_" , integer_to_list(ProcessNumber))),

    timer:sleep(1000),

    update_protocol(),
    OwnReceiver ! {get(protocol), sender_ping},
    receive
        {value, Value} -> got_value_from_receiver
    end,

    NeighborSender ! {protocol, get(protocol)},
    NeighborReceiver ! {get(protocol), ProcessNumber, Value},
    sender(ProcessNumber, ProcessLimit).

update_protocol() ->
    case (get(protocol) == undefined) of
        true -> receive
                    {protocol, Protocol} -> put(protocol, Protocol)
                end;
        false -> do_nothing
    end.

receiver() ->
    receive
        {long_word, sender_ping} ->
            send_longest_word();
        {long_word, _, NeighborWord} ->
            find_longest_word(NeighborWord)
    end,
    receiver().


send_longest_word() ->
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(process_number)))),
    LongestWord = case (get(longest_word) == undefined) of
                      false -> get(longest_word);
                      true -> ""
                  end,
    OwnSender ! {value, LongestWord}.


find_longest_word(NeighborWord) ->
    initialize_longest_word(),
    case (length(NeighborWord) >= length(get(longest_word))) of
        true ->
            update_longest_word(NeighborWord);
        false ->
            do_nothing
    end.


initialize_longest_word() ->
    case (get(longest_word) == undefined) of
        true ->
            compile:file(longest, [debug_info, export_all]),
            put(longest_word, longest:find_longest(get(fragment)));
        false ->
            do_nothing
    end.


update_longest_word(NeighborWord) ->
    LongestWord = get(longest_word),
    ProcessNumber = get(process_number),
    case (length(NeighborWord) == length(LongestWord)) of
        true ->
            io:format("Updated Word at Node ~w: ~s -> ~s~n~n", [ProcessNumber, LongestWord, max(NeighborWord, LongestWord)]),
            put(longest_word, max(NeighborWord, LongestWord));
        false ->
            io:format("Updated Word at Node ~w: ~s -> ~s~n~n", [ProcessNumber, LongestWord, NeighborWord]),
            put(longest_word, NeighborWord)
    end.
