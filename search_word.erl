-module(search_word).
-export([find_word/2]).


find_word(Packet, Word) ->
    Tokens = string:tokens(Packet, " "),
    search_word(Tokens, Word).


search_word(Tokens, Word) when length(Tokens) == 0 ->
    false;
search_word(Tokens, Word) ->
    [Head | Tail] = Tokens,
    case re:run(Head, "[a-zA-Z]+") of
        {match, [{Start, Length}]} ->
		        case Word == string:substr(Head, Start+1, Length) of
                true -> true;
                false -> search_word(Tail, Word)
            end;
        nomatch ->
            search_word(Tail, Word)
    end.
