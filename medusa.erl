-module(medusa).
-export([start/0]).


start() ->
    {ok, [NumOfNodes]} = io:fread("Enter The Number Of Nodes : ", "~d"),
    {ok, [NumOfReplicas]} = io:fread("Enter The Replicas : ", "~d"),
    compile:file(fragment, [debug_info, export_all]),
    Fragments = fragment:get_fragments('data.txt', NumOfNodes, NumOfReplicas),
    create_nodes(NumOfNodes, Fragments),
    select_option(Fragments).


create_nodes(NumOfNodes, Fragments) ->
    compile:file(snake, [debug_info, export_all]),
    create_node(1, NumOfNodes, Fragments).


create_node(NodeNumber, NumOfNodes, _) when NodeNumber > NumOfNodes ->
    done;
create_node(NodeNumber, NumOfNodes, Fragments) ->
    SenderName = list_to_atom(string:concat("snake_sender_", integer_to_list(NodeNumber))),
    ReceiverName = list_to_atom(string:concat("snake_receiver_", integer_to_list(NodeNumber))),
    FragmentNumber = (NodeNumber rem length(Fragments)) + 1,
    register(SenderName, spawn_link(snake, start_sender, [NodeNumber, NumOfNodes])),
    register(ReceiverName, spawn_link(snake, start_receiver, [NodeNumber, FragmentNumber, lists:nth(FragmentNumber, Fragments)])),
    create_node(NodeNumber + 1, NumOfNodes, Fragments).


select_option(Fragments) ->
    io:format("~n1 : Longest Word ~n"),
    io:format("2 : Search a Word ~n"),
    io:format("3 : Find Most Frequent Word ~n"),
    io:format("4 : Update Contents of a Fragment ~n"),
    io:format("5 : Exit the Program ~n~n"),
    {ok, [Choice]} = io:fread("Please Enter Your Choice: ", "~s"),
    case Choice of
        "1" ->
            snake_sender_1 ! {protocol, long_word};
        "2" ->
            {ok, [Word]} = io:fread("Enter the word to be searched : ", "~s"),
            snake_sender_1 ! {protocol, search_word, Word};
        "3" ->
            snake_sender_1 ! {protocol, max_freq};
        "4" ->
            io:format("~nFragment that you want to modify (1 - ~B): ", [length(Fragments)]),
            {ok, [FragmentNumber]} = io:fread("", "~d"),
            io:format("~n---------------------------------------------------------~n~n"),
            io:format("~s~n~n---------------------------------------------------------~n~n", [lists:nth(FragmentNumber, Fragments)]),
            {ok, [OldData]} = io:fread("Replace String: ", "~s"),
            {ok, [NewData]} = io:fread("Replace String With: ", "~s"),
            snake_sender_1 ! {protocol, update_frag, {FragmentNumber, {OldData, NewData}}};
        "5" -> exit("Bye Bye!");
        _ -> io:format("~nPlease Enter the Correct Choice !!~n")
    end,
    timer:sleep(600000).
