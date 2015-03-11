-module(wc_backend_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/diccionario.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

t_add_find_word_test() ->
    {setup,
    fun start/0,
    fun stop/1,
    fun add_find_word_test/1,
    fun(Words) ->
    [Vin,Board] = Words,
    ?assertEqual("bord",Board#wc_word.title),
    ?_assertEqual("vin",Vin#wc_word.title)
    end}.
t_get_all_words_test() ->
    {setup,
    fun start/0,
    fun stop/1,
    fun get_all_words_test/1,
    fun(Words) ->
	    ?_assertNot(length(Words) == 1)
    end}.
t_delete_word_test() ->
    {setup,
    fun start/0,
    fun stop/1,
    fun delete_words_test/1,
    fun(Words)->
     %[ ?assertEqual(W#wc_word.available,true) || W <- Words]
      [Vin,Board] = Words,
      ?assertEqual(Vin#wc_word.available,true),
      ?_assertEqual(Board#wc_word.available,true)      
	    
    end}.


start() ->
    word_collector_app:start(),
    wc_backend:add_word("vin","vino"),
    wc_backend:add_word("bord","mesa").

stop(Words)->
   [wc_mnesia:remove_word(W#wc_word.title)||  W <- Words],
   word_collector_app:stop().

add_find_word_test(_) ->   
    Words = find_words(),
    Words.

get_all_words_test(_)->
    Result = wc_backend:get_all_words(),
    Result.
    
delete_words_test(_)->
    Words = find_words(),
    [wc_backend:delete_word(W#wc_word.title) || W <- Words].
    
find_words()->
    [Vin]   = wc_mnesia:find_word("vin"),
    [Board] = wc_mnesia:find_word("bord"),
    [Vin,Board].
