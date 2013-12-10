-module(snake).
-export([start_sender/2, start_receiver/3]).


%% Starting point for a sender thread. The neighbors are determined here based
%% on the Chord system.
start_sender(NodeNumber, NodeLimit) ->
    compile:file(chord, [debug_info, export_all]),
    put(node_number, NodeNumber),
    put(node_limit, NodeLimit),
    put(neighbor_count, chord:get_neighbor_count(NodeLimit)),
    put(neighbor_list, chord:get_neighbors(NodeNumber, NodeLimit, get(neighbor_count) - 1)),
    sender().


%% Starting point for a receiver thread.
start_receiver(NodeNumber, FragmentId, Fragment) ->
    put(node_number, NodeNumber),
    put(fragment_id, FragmentId),
    put(fragment, Fragment),
    receiver().


%% The sender function randomly chooses a neighbor and sends the updated information.
%% This also takes the updated information from its receiver.
sender() ->
    random:seed(now()),
    OwnReceiver = list_to_atom(string:concat("snake_receiver_", integer_to_list(get(node_number)))),
    timer:sleep(200),
    update_protocol(),

    case get(protocol) of
        max_freq -> Neighbor = integer_to_list(lists:nth(random:uniform(get(node_limit)), lists:seq(1, get(node_limit))));
        _ -> Neighbor = integer_to_list(lists:nth(random:uniform(get(neighbor_count)), get(neighbor_list)))
    end,

    case get(protocol) of
        search_word -> OwnReceiver ! {get(protocol), ping_from_sender, get(word_to_search)};
        update_frag -> OwnReceiver ! {get(protocol), ping_from_sender, {get(fragment_to_be_modified), {get(old_data), get(new_data)}}};
        _ -> OwnReceiver ! {get(protocol), ping_from_sender}
    end,
    receive
        {pong_from_receiver, Value} -> got_value_from_receiver
    end,

    NeighborSender = list_to_atom(string:concat("snake_sender_", Neighbor)),
    NeighborReceiver = list_to_atom(string:concat("snake_receiver_", Neighbor)),

    case get(protocol) of
        search_word -> NeighborSender ! {protocol, search_word, get(word_to_search)};
        update_frag -> NeighborSender ! {protocol, update_frag, Value};
        _ -> NeighborSender ! {protocol, get(protocol)}
    end,
    NeighborReceiver ! {get(protocol), Value},
    sender().


%% Updates the protocol in the sender thread
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


%% Receiver thread that recieves messages from other senders and also carries out the
%% operations depending on the current protocol in force
receiver() ->
    receive
        {long_word, ping_from_sender} ->
            send_longest_word();
        {search_word, ping_from_sender, SearchWord} ->
            send_search_word_result(SearchWord);
        {max_freq, ping_from_sender} ->
            send_frequency_details();
        {update_frag, ping_from_sender, UpdateInformation} ->
            send_update_information(UpdateInformation);
        {long_word, NeighborWord} ->
            find_longest_word(NeighborWord);
        {search_word, {SearchWord, NeighborResult}} ->
            find_search_results(SearchWord, NeighborResult);
        {max_freq, {NeighborDict, GlobalMostFreq, NeighborFragmentId}} ->
            find_updated_frequency(NeighborDict, GlobalMostFreq, NeighborFragmentId);
        {update_frag, {FragId, {OldData, NewData}}} ->
            update_fragment(FragId, OldData, NewData)
    end,
    receiver().

%% Methods to determine the longest word
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
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(node_number)))),
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
    NodeNumber = get(node_number),
    case (length(NeighborWord) == length(LongestWord)) of
        true ->
            io:format("Updated Word at Node ~-4B: ~s -> ~s~n~n", [NodeNumber, LongestWord, max(NeighborWord, LongestWord)]),
            put(longest_word, max(NeighborWord, LongestWord));
        false ->
            io:format("Updated Word at Node ~-4B: ~s -> ~s~n~n", [NodeNumber, LongestWord, NeighborWord]),
            put(longest_word, NeighborWord)
    end.

%% Methods to search all nodes containing the SearchWord
initialize_search_word(SearchWord) ->
    case (get(found_word) == undefined) of
        true ->
            compile:file(search_word, [debug_info, export_all]),
            put(found_word, search_word:find_word(get(fragment), SearchWord)),
            case get(found_word) of
                true -> put(search_results,  [get(node_number)]);
                false -> put(search_results, [])
            end;
        false ->
            do_nothing
    end.


send_search_word_result(SearchWord) ->
    initialize_search_word(SearchWord),
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(node_number)))),
    OwnSender ! {pong_from_receiver, {SearchWord, get(search_results)}}.


find_search_results(SearchWord, NeighborResult) ->
    initialize_search_word(SearchWord),
    case (length(NeighborResult) > 0) of
        true -> update_found_word_result(NeighborResult);
        false -> do_nothing
    end,
    io:format("Search Results at Node ~-4B ===> ~p  ~n~n", [get(node_number), get(search_results)]).


update_found_word_result(NeighborResult) ->
    MyResultSet = sets:from_list(get(search_results)),
    NeighborResultSet = sets:from_list(NeighborResult),
    TotalSet = sets:union(MyResultSet, NeighborResultSet),
    TotalList = sets:to_list(TotalSet),
    put(search_results, TotalList).


%% Methods to determine the highest frequency word in the file
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
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(node_number)))),
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
    io:format("~nMost Frequent Word at Node ~-4B ===> ~s (~B)~n", [get(node_number), Word, Count]).


%% Methods to update the given fragment
send_update_information(Information) ->
    OwnSender = list_to_atom(string:concat("snake_sender_" , integer_to_list(get(node_number)))),
    OwnSender ! {pong_from_receiver, Information}.


update_fragment(FragId, OldData, NewData) ->
    case get(is_fragment_updated) of
        undefined ->
            case get(fragment_id) == FragId of
                true ->
                    io:format("~nNode ~-4B ===> Updating . . . . . .~n", [get(node_number)]),
                    compile:file(update_fragment, [debug_info, export_all]),
                    put(fragment, update_fragment:update_word(get(fragment), OldData, NewData)),
                    io:format("~n------------ Updated Fragment on Node ~B -----------~n~n~s", [get(node_number), get(fragment)]),
                    io:format("~n~n------------------------------------------------~n"),
                    put(is_fragment_updated, true);
                false ->
                    io:format("~nNode ~-4B ===> Not My Fragment~n", [get(node_number)]),
                    put(is_fragment_updated, false)
            end;
        true ->
            io:format("~nNode ~-4B ===> Fragment Already Updated!~n", [get(node_number)]);
        false ->
            io:format("~nNode ~-4B ===> Not My Fragment~n", [get(node_number)])
    end.
