-module(update_fragment).
-export([start/0]).

start() ->
        Packet = "...Heytretrrrrrrrrrrrrrrr$%$#?? you good terminal, Happy Thanksgiving!",
        Old_Word = "Thanksgiving",
        New_Word = "Christmas",
	update_word( Packet, Old_Word, New_Word).

update_word( Packet, Old_Word, New_Word) ->
        Tokens = string:tokens(Packet, " "),
        ResultList = search_word(Tokens, Old_Word,New_Word),
        ResultFrag = string:join( ResultList, " "),
%        io:format("RES ::: ~p~n", [ResultFrag]),
        ResultFrag.


search_word( Tokens, Old_Word, New_Word) when length(Tokens) == 0 ->
        [];

search_word( Tokens, Old_Word, New_Word) ->
        [Head | Tail] = Tokens,
        {match, [{Start, Length}]} = re:run(Head, "[a-zA-Z]+"),
        Token = string:substr(Head, Start+1, Length),
        if Token == Old_Word ->
                List = [New_Word] ++ search_word( Tail, Old_Word,New_Word);
        true ->
                List = [Head] ++ search_word( Tail, Old_Word,New_Word)
        end,
        List.

