-module(snake).
-export([sender/2, start_receiver/4]).


start_receiver(ProcessNumber, ProcessLimit, FragmentId, Fragment) ->
    put(process_number, ProcessNumber),
    put(fragment_id, FragmentId),
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
    OwnReceiver ! {get(protocol), ping_from_sender},
    receive
        {pong_from_receiver, Value} -> got_value_from_receiver
    end,

    NeighborSender ! {protocol, get(protocol)},
    NeighborReceiver ! {get(protocol), ProcessNumber, Value},
    sender(ProcessNumber, ProcessLimit).


update_protocol() ->
    case (get(protocol) == undefined) of
        true ->
            receive
                {protocol, Protocol} -> put(protocol, Protocol);
                {protocol, update_frag, {FragmentNumber, {OldData, NewData}}} ->
                    put(protocol, update_frag),
                    put(fragment_to_be_modified, FragmentNumber),
                    put(old_data, OldData),
                    put(new_data, NewData)
            end;
        false ->
            do_nothing
    end.


receiver() ->
    receive
        {long_word, ping_from_sender} ->
            send_longest_word();
        {max_freq, ping_from_sender} ->
            send_frequency_details();
        {long_word, _, NeighborWord} ->
            find_longest_word(NeighborWord);
        {max_freq, _, {NeighborDict, GlobalMostFreq, NeighborFragmentId,NeighborMaxPair}} ->
            find_updated_frequency(NeighborDict, GlobalMostFreq, NeighborFragmentId,NeighborMaxPair)
        end,
    receiver().


send_frequency_details() ->
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(process_number)))),
    initialize_frequency_details(),
    OwnSender ! {pong_from_receiver, {get(local_dict), get(global_most_frequent), get(fragment_id),get(max_pair)}}.


send_longest_word() ->
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(process_number)))),
    LongestWord = case (get(longest_word) == undefined) of
                      false -> get(longest_word);
                      true -> ""
                  end,
    OwnSender ! {pong_from_receiver, LongestWord}.


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


initialize_frequency_details() ->
    case (get(global_most_frequent) == undefined) of
        true ->
            compile:file(frequency, [debug_info, export_all]),
            put(local_dict, frequency:create_dictionary(get(fragment))),
            put(global_dict, get(local_dict)),
            put(global_most_frequent, frequency:find_most_frequent(get(local_dict))),
	    put(max_pair,frequency:find_most_frequent(get(local_dict))),    
            put(visited_fragment_list, [get(fragment_id)]);
        false ->
            do_nothing
    end.

find_updated_frequency(NeighborDict, NeighborPair, NeighborFragmentId,NeighborMaxPair) ->
    initialize_frequency_details(),
    compile:file(frequency, [debug_info, export_all]),
    case lists:member(NeighborFragmentId, get(visited_fragment_list)) of
        false ->
	    put(global_dict, frequency:merge(get(global_dict), NeighborDict)),
            put(visited_fragment_list, lists:append(get(visited_fragment_list), [NeighborFragmentId])),
	    {K1,V1} = frequency:find_most_frequent(get(global_dict)),
	    {K,V} = NeighborMaxPair,
	    case V > V1 of
		true -> put(global_max_pair,{K,V}),
			io:format("Max Pair ~s: ~B ~n",[K,V]);
		false -> put(global_max_pair,{K1,V1}),
			io:format("Max Pair ~s: ~B ~n",[K1,V1])
	    end;
	    
        true -> do_nothing
    end.

