-module(chord).
-export([get_neighbor_count/1, get_neighbors/3]).


get_neighbors(MyNumber, Limit, 0) ->
    Value = (MyNumber + 1) rem Limit,
    case Value of
        0 -> [Limit];
        _ -> [Value]
    end;
get_neighbors(MyNumber, Limit, Index) ->
    Value = (MyNumber + trunc(math:pow(2, Index))) rem Limit,
    Value1 = case Value of
        0 -> Limit;
        _ -> Value
    end,
    [Value1 | get_neighbors(MyNumber, Limit, Index - 1)].


get_neighbor_count(Value) ->
    TruncatedValue = erlang:trunc(log2(Value)),
    case TruncatedValue == Value of
        true -> TruncatedValue;
        false -> TruncatedValue + 1
    end.


log2(Value) ->
    math:log(Value) / math:log(2).
