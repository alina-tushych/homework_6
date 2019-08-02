-module(help_funs).

-export([
    get_time_now_as_seconds/0,
    delete_obsolete/1,
    lookup_by_range/4
]).


get_time_now_as_seconds() -> 
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).


delete_obsolete(TableName) ->
    FirstKey = ets:first(TableName),
    NowSeconds = get_time_now_as_seconds(),
    ok = delete_obsolete(TableName, FirstKey, NowSeconds).

delete_obsolete(_TableName, '$end_of_table', _NowSeconds) ->  
    ok;

delete_obsolete(TableName, Key, NowSeconds) -> 
    NextKey = ets:next(TableName, Key),
    KVList = ets:lookup(TableName, Key),      
    case KVList of
        [{Key, _Value, _StartAsSeconds, EndDateSeconds}] when NowSeconds >= EndDateSeconds -> 
            ets:delete(TableName, Key);
        _ -> true
    end, 
    delete_obsolete(TableName, NextKey, NowSeconds).


lookup_by_range(TableName, FirstKey, DateFrom, DateTo) ->
    lookup_by_range(TableName, FirstKey, DateFrom, DateTo, []).

lookup_by_range(_TableName, '$end_of_table', _DateFrom, _DateTo, AccList) -> AccList;

lookup_by_range(TableName, Key, DateFrom, DateTo, AccList) ->
    NextKey = ets:next(TableName, Key),
    KVList = ets:lookup(TableName, Key),
    NewAccList = case KVList of
        [{Key, Value, StartAsSeconds, _EndDateSeconds}] when StartAsSeconds >= DateFrom andalso  StartAsSeconds =< DateTo ->
            [{Key, Value} | AccList];
        _ -> AccList
    end,
    lookup_by_range(TableName, NextKey, DateFrom, DateTo, NewAccList).
