-module(cache_server_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [
    test_start_link,
    test_insert,
    test_lookup,
    test_lookup_by_date
].


init_per_suite(Config) ->
    Config.

init_per_testcase(test_start_link, Config) ->
    Test1 = #{
            <<"table_name">> => a,
            <<"drop_interval">> => 5
    },
    [{<<"start_link">>, Test1} | Config];

init_per_testcase(test_insert, Config) ->
    Test2 = #{
            <<"table_name">> => a,
            <<"key">> => gh,
            <<"value">> => kl,
            <<"ttl">> => 10
    },
    [{<<"insert">>, Test2} | Config];
    
init_per_testcase(test_lookup, Config) ->
    Test3 = #{
            <<"table_name">> => a,
            <<"key">> => gh2
    },
    [{<<"lookup">>, Test3} | Config];

init_per_testcase(test_lookup_by_date, Config) ->
    Test4 = #{
            <<"table_name">> => a,
            <<"date_from">> => {{2015,1,1},{00,00,00}},
            <<"date_to">> => {{2015,1,10},{23,59,59}}
    },
    [{<<"lookup_by_date">>, Test4} | Config].

end_per_testcase(test_start_link, Config) ->
    Config;

end_per_testcase(test_insert, Config) ->
    Config;

end_per_testcase(test_lookup, Config) ->
    Config;

end_per_testcase(test_lookup_by_date, Config) ->
    Config.

end_per_suite(Config) ->
    Config.


test_start_link(Config) ->
    Map = proplists:get_value(<<"start_link">>, Config),
    TableName = maps:get(<<"table_name">>, Map),
    DropInterval = maps:get(<<"drop_interval">>, Map),    
    {ok, Pid} = cache_server:start_link(TableName, [{drop_interval, DropInterval}]),
    true = is_pid(Pid).

test_insert(Config) ->
    Map = proplists:get_value(<<"insert">>, Config),
    TableName = maps:get(<<"table_name">>, Map),
    Key = maps:get(<<"key">>, Map), 
    Value = maps:get(<<"value">>, Map),  
    TTL = maps:get(<<"ttl">>, Map),   
    ?_assertEqual(
        ok, 
        cache_server:insert(TableName, Key, Value, TTL)
    ).

test_lookup(Config) ->
    Map = proplists:get_value(<<"lookup">>, Config),
    TableName = maps:get(<<"table_name">>, Map),
    Key = maps:get(<<"key">>, Map),   
    ?_assertEqual(
        undefined,
        cache_server:lookup(TableName, Key)
    ).

test_lookup_by_date(Config) ->
    Map = proplists:get_value(<<"lookup_by_date">>, Config),
    TableName = maps:get(<<"table_name">>, Map),
    DateFrom = maps:get(<<"date_from">>, Map), 
    DateTo = maps:get(<<"date_to">>, Map),
    ?_assertEqual(
        {ok, []},
        cache_server:lookup_by_date(TableName, DateFrom, DateTo)
    ).
