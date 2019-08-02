-module(cache_server_sup).

-behaviour(supervisor).


-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    T = {cache_server, {cache_server, start_link, [test_table, [{drop_interval, 100}]]}, permanent, 2000, worker, [cache_server]},
    {ok, {{one_for_one, 500, 25}, [T]}}.

