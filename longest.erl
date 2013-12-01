-module(longest).
-export([find_longest/1]).

find_longest(Packet) ->
    Tokens = string:tokens(Packet, " "),
    Longest = strip_word(Tokens),
    {match, [{Start, Length}]} = re:run(Longest, "[a-zA-Z]+"),
    string:substr(Longest, Start+1, Length).


strip_word(Tokens) when length(Tokens) == 1 ->
    lists:last(Tokens);
strip_word(Tokens) ->
    [Head | Tail] = Tokens,
    case re:run(Head, "[a-zA-Z]+") of
        {match, [{_, Length}]} ->
            Max = strip_word(Tail),
            if length(Max) > Length ->
                    Max;
               true ->
                    Head
            end;
        nomatch -> error
    end.
