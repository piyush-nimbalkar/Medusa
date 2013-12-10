-module(frequency).
-export([create_dictionary/1, find_most_frequent/1, merge/2]).


merge(Dict1, Dict2) ->
    dict:fold(fun(K2, V2, D1) ->
                      case (dict:find(K2, D1)) of
                          {ok, _} -> dict:update(K2, fun(V1) -> V1 + V2 end, D1);
                          error -> D1
                      end
              end, Dict1, Dict2).


create_dictionary(Packet) ->
    Tokens = string:tokens(Packet, " "),
    lists:foldl(fun(Word, Dict) ->
                        case re:run(Word, "[a-zA-Z]+") of
                            {match, [{Start, Length}]} ->
                                RefinedWord = string:substr(Word, Start+1, Length),
                                dict:update(RefinedWord, fun(Count) -> Count + 1 end, 1, Dict);
                            nomatch -> Dict
                        end
                end, dict:new(), Tokens).


find_most_frequent(Dict) ->
    dict:fold(fun(K, V, {MaxKey, MaxValue}) ->
                      case (V > MaxValue) of
                          true -> {K, V};
                          false -> {MaxKey, MaxValue}
                      end
              end, {non_existing_key, 0}, Dict).

print_dict(Dict) ->
    dict:fold(fun(Word, Count, AccIn) ->
                      io:format("~s: ~w~n", [Word, Count]),
                      AccIn
              end, void, Dict),
    io:format("------------------~n").
