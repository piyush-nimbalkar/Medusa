-module(chord).
-export([get_neighbor_count/1, get_neighbors/2]).


get_neighbors(MyNumber, 0) ->
    [(MyNumber + 1) rem get(process_limit) + 1];
get_neighbors(MyNumber, Index) ->
    [(MyNumber + trunc(math:pow(2, Index))) rem get(process_limit) + 1 | get_neighbors(MyNumber, Index - 1)].


get_neighbor_count(Value) ->
    TruncatedValue = erlang:trunc(log2(Value)),
    case TruncatedValue == Value of
        true -> TruncatedValue;
        false -> TruncatedValue + 1
    end.


log2(Value) ->
    math:log(Value) / math:log(2).
