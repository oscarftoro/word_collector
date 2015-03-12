-module(wc_backend_SUITE).
-author("oscar_toro").
-include_lib("common_test/include/ct.hrl").
-include("../include/diccionario.hrl").
-export([all/0,groups/0,init_per_suite/1,end_per_suite/1,init_per_group/2,end_per_group/2]).
%% tests
-export([add_word_test/1,find_word_test/1]).
all()->
    [{group,addFind}].

groups() ->
  [{addFind,[sequence],[add_word_test,find_word_test]}].

init_per_suite(Config) ->
  application:load(sasl),
  application:set_env(sasl,sasl_error_logger,false),
  application:set_env(sasl,errlog_type,error),
  error_logger:tty(false),
  ok = application:start(sasl),
  wc_mnesia:check_db_exist([node()]),
  word_collector_app:start(),
  Config.

end_per_suite(_Config)->
  application:stop(word_collector),
  application:stop(sasl),
  ok.

init_per_group(_,Config) ->
  Config.
end_per_group(addFind,Config)->
  lists:foreach(fun(W) -> wc_mnesia:remove_word(W) end,Config).

add_word_test(_Config) ->
  wc_backend:add_word("vin","vino"),
  wc_backend:add_word("bord","mesa").

find_word_test(_Config) ->
  [Vin]   = wc_mnesia:find_word("vin"),
  [Board] = wc_mnesia:find_word("bord"),
  [Vin,Board].
