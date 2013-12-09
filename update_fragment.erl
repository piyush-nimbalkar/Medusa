-module(update_fragment).
-export([update_word/3]).


update_word(Packet, Old_Word, New_Word) ->
    Tokens = string:tokens(Packet, " "),
    ResultList = replace_word(Tokens, Old_Word, New_Word),
    string:join(ResultList, " ").


replace_word(Tokens, Old_Word, New_Word) when length(Tokens) == 0 ->
    [];
replace_word(Tokens, Old_Word, New_Word) ->
    [Head | Tail] = Tokens,
    case re:run(Head, "[a-zA-Z]+") of
        {match, [{Start, Length}]} ->
		        Token = string:substr(Head, Start+1, Length),
        		if Token == Old_Word ->
        	        	[New_Word] ++ replace_word(Tail, Old_Word, New_Word);
               true ->
        		        [Head] ++ replace_word(Tail, Old_Word, New_Word)
        		end;
        nomatch ->
            [Head] ++ replace_word(Tail, Old_Word, New_Word)
    end.
