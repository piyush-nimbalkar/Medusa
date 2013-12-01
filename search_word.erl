-module(search_word).
-export([start/0]).

start() ->
	Packet = "...Heytretrrrrrrrrrrrrrrr$%$#?? you good terminal, Happy Thanksgiving!",
	Word = "Thanksgiving",
	find_word(Packet, Word).

find_word(Packet, Word) ->
	Tokens = string:tokens(Packet, " "),
	Result = search_word(Tokens, Word),
        io:format("Result : ~s~n",[Result]).

search_word(Tokens, Word) when length(Tokens) == 0 ->
	false;

search_word( Tokens, Word) ->
	[Head | Tail] = Tokens,
	{match, [{Start, Length}]} = re:run(Head, "[a-z,A-Z]+"),
	Token = string:substr(Head, Start+1, Length),
        if Token == Word ->
		true;
	true ->
		Result = search_word( Tail, Word),
		Result
	end.

