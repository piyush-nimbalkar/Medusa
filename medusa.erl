-module(medusa).
-export([start/0, create_nodes/0]).


create_nodes() ->
    compile:file(snake, [debug_info, export_all]),
    create_node(8000, 8010).


create_node(Port, PortEnd) ->
    if Port > PortEnd ->
            done;
       true ->
            spawn(snake, start, [Port]),
            create_node(Port + 2, PortEnd)
    end.


start() ->
    Tokens = read_file("data.txt"),
    NumFragments = 10,
    WordLength  = length(Tokens),
    FragSize = WordLength div (NumFragments-1),
    io:format("~p~n",[make_sublists(FragSize, Tokens)]).


read_file(FileName) ->
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
