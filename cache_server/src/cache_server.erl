-module(cache_server).

-behaviour(gen_server).


-export([
    start_link/2,
    insert/4,
    lookup/2,
    lookup_by_date/3
]).


-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link(TableName, DropIntervalProp) ->
    DropInterval = proplists:get_value(drop_interval, DropIntervalProp),
    InitParamsMaps = #{table_name => TableName, drop_interval => DropInterval},
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitParamsMaps, []).

init(InitParamsMaps) ->
    TableName = maps:get(table_name, InitParamsMaps),
    DropInterval = maps:get(drop_interval, InitParamsMaps),
    ets:new(TableName, [public, named_table]),
    timer:apply_interval(DropInterval*1000, help_funs, delete_obsolete, [TableName]),
    {ok, #state{}}.

insert(TableName, Key, Value, TTL) ->
    gen_server:call(?MODULE, {insert, TableName, Key, Value, TTL}).

lookup(TableName, Key) ->
    gen_server:call(?MODULE, {lookup, TableName, Key}).

lookup_by_date(TableName, DateFrom, DateTo) ->
    gen_server:call(?MODULE, {lookup_by_date, TableName, DateFrom, DateTo}).


handle_call({insert, TableName, Key, Value, TTL}, _From, State) ->
    StartAsSeconds = help_funs:get_time_now_as_seconds(),
    EndDateSeconds = StartAsSeconds + TTL,
    ets:insert(TableName, {Key, Value, StartAsSeconds, EndDateSeconds}),
    Reply = ok,
    {reply, Reply, State};

handle_call({lookup, TableName, Key}, _From, State) -> 
    KVList = ets:lookup(TableName, Key),
    NowSeconds = help_funs:get_time_now_as_seconds(),
    Reply = case KVList of
        [{Key, Value, _StartAsSeconds, EndDateSeconds}] when NowSeconds =< EndDateSeconds -> 
            {ok, Value};
        _ -> undefined
    end,
    {reply, Reply, State};

handle_call({lookup_by_date, TableName, DateFrom, DateTo}, _From, State) ->
    DateFromAsSeconds = calendar:datetime_to_gregorian_seconds(DateFrom),
    DateToAsSeconds = calendar:datetime_to_gregorian_seconds(DateTo),
    FirstKey = ets:first(TableName),
    KeyValueList = help_funs:lookup_by_range(TableName, FirstKey, DateFromAsSeconds, DateToAsSeconds),
    Reply = {ok, KeyValueList},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
