-module(wc_backend_SUITE).
-author("oscar_toro").
-include_lib("common_test/include/ct.hrl").
-include("../include/diccionario.hrl").
-export([all/0,groups/0,init_per_suite/1,
  end_per_suite/1,init_per_group/2,end_per_group/2,
  init_per_testcase/2,end_per_testcase/2
]).
%% tests
-export([add_word_test/1,find_word_test/1,
  get_all_words_test/1,delete_words_test/1]).
-define(DEBUG(X),io:format("DEBUG ~p: ~p ~p~n",[?MODULE,?LINE,X])).
all()->
    [{group,addFind},{group,getAllDelete}].

groups() ->
  [{addFind,[sequence],[add_word_test,find_word_test]},
  {getAllDelete,[sequence],[get_all_words_test,delete_words_test]}].

init_per_suite(Config) ->
 
  Config.

end_per_suite(_Config)->
    ok.
init_per_group(getAllDelete,Config)->
  wc_backend:add_word(<<"peter">>,<<"pedro">>),
  wc_backend:add_word(<<"mobiltelefon">>,<<"celular">>),
  Config;

init_per_group(_,Config) ->
  Config.

end_per_group(getAllDelete,Config) ->
  wc_mnesia:remove_word(<<"peter">>),
  wc_mnesia:remove_word(<<"mobiltelefon">>),
  Config;

end_per_group(addFind,Config)->
  lists:foreach(fun(W) -> 
    wc_mnesia:remove_word(W) 
  end,[<<"vin">>,<<"bord">>]),  
  Config.

init_per_testcase(_,Config)->
  Config.

end_per_testcase(get_all_words_test,_Config) ->
  wc_mnesia:remove_word(<<"uge">>);

end_per_testcase(_,Config) ->
  Config.

add_word_test(Config) ->
  wc_backend:add_word(<<"vin">>,<<"vino">>),
  wc_backend:add_word(<<"bord">>,<<"mesa">>),
  Config.

find_word_test(Config) ->

  [Vin]   = wc_backend:find_word(<<"vin">>),
  [Board] = wc_backend:find_word(<<"bord">>),
  <<"vin">>   = Vin#wc_word.title,
  <<"bord">>  = Board#wc_word.title,
  Config.

get_all_words_test(Config)->
  Result = wc_backend:get_all_words(),
  ?DEBUG(Result),
  2 = length(Result),
  
  wc_backend:add_word(<<"uge">>, <<"semana">>),
  Result2 = wc_backend:get_all_words(),
  3 = length(Result2),
  Config.

%todo: check that available is false
delete_words_test(_Config)->
  wc_backend:delete_word(<<"peter">>),
  1 = length(wc_backend:get_all_words()),
  wc_backend:delete_word(<<"mobiltelefon">>),
  0 = length(wc_backend:get_all_words()).
 
