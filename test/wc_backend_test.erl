-module(wc_backend_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/diccionario.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

generator_add_find_word_test() ->
    {setup,
    fun start/0,
    fun stop/1,
    fun add_find_word_test/1,
    fun(Words) ->
    [Vin,Board] = Words,
    ?assertEqual("bord",Board#wc_word.title),
    ?assertEqual("vin",Vin#wc_word.title)
    end}.

start() ->
    word_collector_app:start().
   
  
stop(Words)->
   [Vin,Board] = Words,
   wc_mnesia:remove_word(Vin#wc_word.title),
   wc_mnesia:remove_word(Board#wc_word.title),
   word_collector_app:stop().

add_find_word_test(_) ->
    wc_backend:add_word("vin","vino"),
    wc_backend:add_word("bord","mesa"),
    [Vin]   = wc_mnesia:find_word("vin"),
    [Board] = wc_mnesia:find_word("bord"),
    [Vin,Board].

