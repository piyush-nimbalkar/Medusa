-module(fragment).
-export([get_fragments/3]).


get_fragments(FileName, NumOfNodes, NumOfReplicas) ->
    Tokens = read_file(FileName),
    FileLength = length(Tokens),
    FragSize = NumOfReplicas * (FileLength div (NumOfNodes - 1)),
    make_sublists(FragSize, Tokens).


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
