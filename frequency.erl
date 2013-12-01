-module(frequency).
-export([start/0]).

start() ->
        Packet = "...Heytretrrrrrrrrrrrrrrr$%$#?? you good you you terminal, Happy Thanksgiving!",
	Packet2 = "good terminal when is Thanksgiving!",
	Dict1 = find_frequency(Packet),
	Dict2 = find_frequency(Packet2),
	D3 = merge(Dict1,Dict2),
	print_dict(D3).

merge(D1,D2) ->
	D3 = dict:merge(fun(K,V1,V2) -> V1+V2 end , D1,D2).

find_frequency(Packet) ->
	Tokens = string:tokens(Packet, " "),
	Dict = lists:foldl(
                        fun(W, D) -> 
       				{match, [{Start, Length}]} = re:run(W, "[a-zA-Z]+"),
        			Word = string:substr(W, Start+1, Length),
				dict:update(Word, fun(C) -> C + 1 end, 1, D)
				 end, 
                        dict:new(), 
                        Tokens).

print_dict(Dict) ->
    dict:fold(fun(Word, Count, AccIn) -> 
		io:format("~s: ~w~n", [Word, Count]), AccIn end, void, Dict).
