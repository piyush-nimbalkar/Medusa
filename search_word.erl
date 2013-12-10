-module(search_word).

%% Find given word in packet
find_word(Packet, Word) ->
	Tokens = string:tokens(Packet, " "),
	Result = search_word(Tokens, Word),
        Result.

search_word(Tokens, Word) when length(Tokens) == 0 ->
	false;

search_word( Tokens, Word) ->
	[Head | Tail] = Tokens,
%	{match, [{Start, Length}]} = re:run(Head, "[a-zA-Z]+"),
	case re:run(Head, "[a-zA-Z]+") of
		{match, [{Start, Length}]} ->	
			Token = string:substr(Head, Start+1, Length),
		        if Token == Word ->
				true;
			true ->
				Result = search_word( Tail, Word),
				Result
			end;
		nomatch ->
			Result = search_word( Tail, Word),
                        Result
	end.
