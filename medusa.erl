-module(medusa).
-export([start/0]).


start() ->
    NumOfNodes = 10,
    NumOfReplicas = 2,
    Tokens = read_file("data.txt"),
    FileLength  = length(Tokens),
    FragSize = NumOfReplicas * (FileLength div (NumOfNodes - 1)),
    Fragments = make_sublists(FragSize, Tokens),
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
    register(SenderName, spawn_link(snake, sender, [NodeNumber, NumOfNodes])),
    register(ReceiverName, spawn_link(snake, start_receiver, [NodeNumber, NumOfNodes, FragmentNumber, lists:nth(FragmentNumber, Fragments)])),
    create_node(NodeNumber + 1, NumOfNodes, Fragments).


select_option(Fragments) ->
    io:format("~n1 : Longest Word ~n"),
    io:format("2 : Search a Word ~n"),
    io:format("3 : Find Most Frequent Word ~n"),
    io:format("4 : Update Contents of a Fragment ~n"),
    io:format("5 : Exit the Program ~n"),
    {ok, [Choice]} = io:fread("Please Enter Your Choice: ", "~s"),
    case Choice of
        "1" ->
            snake_sender_1 ! {protocol, long_word};
        "2" ->
            {ok, [W]} = io:fread("Enter the word to be searched : ", "~s"),
            io:format("Word is : ~s~n",[W]);
        "3" ->
            snake_sender_1 ! {protocol, max_freq};
        "4" ->
            io:format("Fragment that you want to modify (1 - ~B): ", [length(Fragments)]),
            {ok, [FragmentNumber]} = io:fread("", "~d"),
            io:format("---------------------------------------------------------~n"),
            io:format("~s~n---------------------------------------------------------~n", [lists:nth(FragmentNumber, Fragments)]),
            {ok, [OldData]} = io:fread("Replace String: ", "~s"),
            {ok, [NewData]} = io:fread("Replace String With: ", "~s"),
            snake_sender_1 ! {protocol, update_frag, {FragmentNumber, {OldData, NewData}}};
        "5" -> exit("Bye Bye!");
        _ -> io:format("~nPlease Enter the Correct Choice !!~n")
    end,
    timer:sleep(600000).


read_file(FileName)  ->
    {ok, Device} = file:open(FileName, [read]),
    List1 = [],
    Tokens = count_lines(Device, List1),
    Tokens.


count_lines(Device,  List1) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device),
                List1;
        Line -> Tokens = string:tokens(Line, " "),
                List3 = lists:append(List1, Tokens),
                count_lines(Device, List3)
    end.


make_sublists(FragSize, Tokens) ->
    {Fragment, Remaining} = if length(Tokens) >= FragSize ->
                                    lists:split(FragSize, Tokens);
                               true ->
                                    {Tokens, []}
                            end,
    JoinedFrag = lists:foldl(fun(Item, Accumulation) ->
                                     string:join([Accumulation, Item], " ")
                             end, "", Fragment),
    if Remaining == [] ->
            [JoinedFrag];
       true ->
            UltimateList = make_sublists(FragSize, Remaining),
            lists:append(UltimateList, [JoinedFrag])
    end.
