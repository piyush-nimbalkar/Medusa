-module(update_fragment).
-export([start/0]).

start() ->
        Packet = "...Heytretrrrrrrrrrrrrrrr$%$#?? you good terminal, Happy Thanksgiving!",
        Old_Word = "Thanksgiving",
        New_Word = "Christmas",
	find_word(Packet, Old_Word,New_Word).

find_word(Packet, Old_Word, New_Word) ->
        Tokens = string:tokens(Packet, " "),
        Result = search_word(Tokens, Old_Word,New_Word),
        io:format("Result : ~s~n",[Result]).

search_word(Tokens, Old_Word, New_Word) when length(Tokens) == 0 ->
        false;

search_word( Tokens, Old_Word, New_Word) ->
        [Head | Tail] = Tokens,
        {match, [{Start, Length}]} = re:run(Head, "[a-zA-Z]+"),
        Token = string:substr(Head, Start+1, Length),
        if Token == Old_Word ->
		io:format("~p~n",[Head]),
%		Head = New_Word,
                Head;
        true ->
                Result = search_word( Tail, Old_Word,New_Word),
                Result
        end.

