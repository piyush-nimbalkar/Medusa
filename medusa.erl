-module(medusa).
-export([start/0, create_nodes/0]).


create_nodes() ->
    compile:file(snake, [debug_info, export_all]),
    create_node(1, 10),
    timer:sleep(20000).


create_node(ProcessNumber, ProcessLimit) when ProcessNumber > ProcessLimit ->
    done;
create_node(ProcessNumber, ProcessLimit) ->
    SenderName = list_to_atom(string:concat("snake_sender_", integer_to_list(ProcessNumber))),
    ReceiverName = list_to_atom(string:concat("snake_receiver_", integer_to_list(ProcessNumber))),
    register(SenderName, spawn_link(snake, sender, [ProcessNumber, ProcessLimit])),
    register(ReceiverName, spawn_link(snake, receiver, [ProcessNumber, ProcessLimit])),
    create_node(ProcessNumber + 1, ProcessLimit).


start() ->
    Tokens = read_file("data.txt"),
    NumFragments = 10,
    WordLength  = length(Tokens),
    FragSize = WordLength div (NumFragments-1),
    io:format("~p~n",[make_sublists(FragSize, Tokens)]),
    select_ur_option().


select_ur_option() ->    
    io:format("~n1 : Longest Word ~n"),
    io:format("2 : Search a word ~n"),
    io:format("3 : Find most frequent word ~n"),
    io:format("4 : Exit the program ~n"),
    io:format("Please enter your choice !!!~n"),
    {ok, [X]} = io:fread("input : ", "~s"),
    io:format("~p~n",[X]),
    case X of
        "1" -> io:format("In 1");
        "2" -> 
          {ok, [W]} = io:fread("Enter the word to be searched : ", "~s"),
          io:format("Word is : ~s~n",[W]);
	"3" -> io:format("In 3");
        "4" -> exit(self(),normal);
	Check -> io:format("Please enter the correct choice!!~n")
    end,
    select_ur_option().


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
