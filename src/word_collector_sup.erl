-module(word_collector_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	%%Procs = [{wc_backend,{wc_backend,start_link(),[]},
        %%permanent,1000,worker,[wc_backend,wc_mnesia]}],
	{ok, {{one_for_one, 1, 5}, [child(wc_backend,none)]}}.

child(Module, none) ->
        {Module, {Module, start_link, []}, permanent, brutal_kill, worker, [Module]}.
%% child(Module, Name) ->
%%         {{Module, Name}, {Module, start_link, [Name]},
%%                 permanent, brutal_kill, worker, [Module]}.
