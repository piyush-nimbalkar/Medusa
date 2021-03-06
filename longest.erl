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
            MaxWord = strip_word(Tail),
            if length(MaxWord) >= Length ->
                    case length(MaxWord) == Length of
                        true -> max(Head, MaxWord);
                        false -> MaxWord
                    end;
               true ->
                    Head
            end;
        nomatch ->
            strip_word(Tail)
    end.
