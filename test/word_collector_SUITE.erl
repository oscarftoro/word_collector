-module(word_collector_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([init_per_suite/1]).

all() ->
    [].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia,dir,Priv),
    word_collector_app:install([node()]),
    word_collector_app:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.