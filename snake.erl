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
    timer:sleep(500),
    update_protocol(),

    case get(protocol) of
        search_word -> OwnReceiver ! {get(protocol), ping_from_sender, get(word_to_search)};
        _ -> OwnReceiver ! {get(protocol), ping_from_sender}
    end,

    receive
        {pong_from_receiver, Value} -> got_value_from_receiver
    end,

    case get(protocol) of
        search_word -> NeighborSender ! {protocol, search_word, get(word_to_search)};
        _ -> NeighborSender ! {protocol, get(protocol)}
    end,

    NeighborReceiver ! {get(protocol), ProcessNumber, Value},
    sender(ProcessNumber, ProcessLimit).


update_protocol() ->
    case (get(protocol) == undefined) of
        true ->
            receive
                {protocol, Protocol} ->
                    put(protocol, Protocol);
                {protocol, search_word, Word} ->
                    put(protocol, search_word),
                    put(word_to_search, Word);
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
        {search_word, ping_from_sender, SearchWord} ->
            send_search_word_result(SearchWord);
        {max_freq, ping_from_sender} ->
            send_frequency_details();
        {update_frag, ping_from_sender} ->
            send_updated_fragment();
        {long_word, _, NeighborWord} ->
            find_longest_word(NeighborWord);
        {search_word, _, {SearchWord, NeighborResult}} ->
            find_search_results(SearchWord, NeighborResult);
        {max_freq, _, {NeighborDict, GlobalMostFreq, NeighborFragmentId}} ->
            find_updated_frequency(NeighborDict, GlobalMostFreq, NeighborFragmentId);
        {update_frag, _, {FragId, OldData, NewData}} ->
            find_updated_frag(FragId, OldData, NewData)
    end,
    receiver().


initialize_longest_word() ->
    case (get(longest_word) == undefined) of
        true ->
            compile:file(longest, [debug_info, export_all]),
            put(longest_word, longest:find_longest(get(fragment)));
        false ->
            do_nothing
    end.


send_longest_word() ->
    initialize_longest_word(),
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(process_number)))),
    LongestWord = get(longest_word),
    OwnSender ! {pong_from_receiver, LongestWord}.


find_longest_word(NeighborWord) ->
    initialize_longest_word(),
    case (length(NeighborWord) >= length(get(longest_word))) of
        true -> update_longest_word(NeighborWord);
        false -> do_nothing
    end.


update_longest_word(NeighborWord) ->
    LongestWord = get(longest_word),
    ProcessNumber = get(process_number),
    case (length(NeighborWord) == length(LongestWord)) of
        true ->
            io:format("Updated Word at Node ~-4B: ~s -> ~s~n~n", [ProcessNumber, LongestWord, max(NeighborWord, LongestWord)]),
            put(longest_word, max(NeighborWord, LongestWord));
        false ->
            io:format("Updated Word at Node ~-4B: ~s -> ~s~n~n", [ProcessNumber, LongestWord, NeighborWord]),
            put(longest_word, NeighborWord)
    end.


initialize_search_word(SearchWord) ->
    case (get(found_word) == undefined) of
        true ->
            compile:file(search_word, [debug_info, export_all]),
            put(found_word, search_word:find_word(get(fragment), SearchWord)),
            case get(found_word) of
                true -> put(search_results,  [get(process_number)]);
                false -> put(search_results, [])
            end;
        false ->
            do_nothing
    end.


send_search_word_result(SearchWord) ->
    initialize_search_word(SearchWord),
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(process_number)))),
    OwnSender ! {pong_from_receiver, {SearchWord, get(search_results)}}.


find_search_results(SearchWord, NeighborResult) ->
    initialize_search_word(SearchWord),
    case (length(NeighborResult) > 0) of
        true -> update_found_word_result(NeighborResult);
        false -> do_nothing
    end,
    io:format("Search Results at Node ~-4B ===> ~p  ~n~n", [get(process_number), get(search_results)]).


update_found_word_result(NeighborResult) ->
    ProcessNumber = get(process_number),
    MyResultSet = sets:from_list(get(search_results)),
    NeighborResultSet = sets:from_list(NeighborResult),
    TotalSet = sets:union(MyResultSet, NeighborResultSet),
    TotalList = sets:to_list(TotalSet),
    put(search_results, TotalList).


initialize_frequency_details() ->
    case (get(global_most_frequent) == undefined) of
        true ->
            compile:file(frequency, [debug_info, export_all]),
            put(local_dict, frequency:create_dictionary(get(fragment))),
            put(global_dict, get(local_dict)),
            put(global_most_frequent, frequency:find_most_frequent(get(global_dict))),
            put(visited_fragment_list, [get(fragment_id)]);
        false ->
            do_nothing
    end.


send_frequency_details() ->
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(process_number)))),
    initialize_frequency_details(),
    OwnSender ! {pong_from_receiver, {get(local_dict), get(global_most_frequent), get(fragment_id)}}.


find_updated_frequency(NeighborDict, NeighborFreqPair, NeighborFragmentId) ->
    initialize_frequency_details(),
    compile:file(frequency, [debug_info, export_all]),
    case lists:member(NeighborFragmentId, get(visited_fragment_list)) of
        false ->
            put(global_dict, frequency:merge(get(global_dict), NeighborDict)),
            put(visited_fragment_list, lists:append(get(visited_fragment_list), [NeighborFragmentId]));
        true -> do_nothing
    end,

    MyFreqPair = frequency:find_most_frequent(get(global_dict)),
    {MyFreqWord, MyFreqCount} = MyFreqPair,
    {NeighborFreqWord, NeighborFreqCount} = NeighborFreqPair,

    case (MyFreqCount =< NeighborFreqCount) of
        true -> case (MyFreqCount == NeighborFreqCount) of
                    true -> case (max(MyFreqWord, NeighborFreqWord)) of
                                MyFreqWord -> put(global_most_frequent, MyFreqPair);
                                NeighborFreqWord -> put(global_most_frequent, NeighborFreqPair)
                            end;
                    false -> put(global_most_frequent, NeighborFreqPair)
                end;
        false -> put(global_most_frequent, MyFreqPair)
    end,
    {Word, Count} = get(global_most_frequent),
    io:format("~nMost Frequent Word at Node ~-4B ===> ~s (~B)~n", [get(process_number), Word, Count]).


initialize_updated_fragment() ->
    case (get(updated_fragment) == undefined) of
        true ->
            compile:file(update_fragment, [debug_info, export_all]),
                                                %	    case (get(fragment_id) == get(fragment_to_be_modified)) of
                                                %		true ->  io:format("in 2nd true~n"),%put(updated_fragment,update_fragment:update_word(get(fragment),get(old_data),get(new_data)));
            put(fragment,update_fragment:update_word(get(fragment), "Krishna","Shiva"));
                                                %
                                                %		false -> io:format("word not found in fragment~n")
                                                %	    end;
        false -> do_nothing
    end.


send_updated_fragment() ->
    initialize_updated_fragment(),
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(process_number)))),
    OwnSender ! {pong_from_receiver,{get(fragment_id),get(updated_fragment)}}.


find_updated_frag(FragId,OldData,NewData) ->
    initialize_updated_fragment().
