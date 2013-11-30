-module(longest).
-export([start/0]).

start() ->
	Packet = "...Heytretrrrrrrrrrrrrrrr$%$#?? you good terminal, Happy Thanksgiving!",
	find_longest(Packet).

find_longest(Packet) ->
        Tokens = string:tokens(Packet, " "),
	Longest = strip_word(Tokens),
	{match, [{Start, Length}]} = re:run(Longest, "[a-z,A-Z]+"),
	Word = string:substr(Longest, Start+1, Length),
	io:format("~s~n",[Word]).


strip_word(Tokens) when length(Tokens) == 1 ->
	lists:last(Tokens);
strip_word(Tokens) ->
	[Head | Tail] = Tokens,
        case re:run(Head, "[a-z,A-Z]+") of
             	{match, [{_, Length}]} ->
			Max = strip_word(Tail),
			if length(Max) > Length ->
				Max;
			true ->
				Head
			end;
		nomatch -> error
        end.

