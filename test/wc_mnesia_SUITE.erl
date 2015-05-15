-module(wc_mnesia_SUITE).
-author("oscar_toro").
-include_lib("common_test/include/ct.hrl").
-include("../include/diccionario.hrl").
-export([init_per_suite/1,end_per_suite/1,init_per_testcase/2,
  end_per_testcase/2,  all/0,add_word/1,
  get_all_words/1,get_all_words2/1,find_word_by_name/1, 
  edit_word/1,delete_word/1]).

all() ->  
  [add_word,get_all_words,find_word_by_name,
  get_all_words2,edit_word,delete_word].

init_per_suite(Config) ->

  Config.

end_per_suite(_Config) ->
  
  application:stop(sasl),
  hackney:stop(),
  word_collector_app:stop(),
  application:stop(mnesia),
  ok.

init_per_testcase(find_word_by_name,Config) ->
  
  {atomic,ok} = wc_mnesia:add_word(<<"blondine">>,<<"rubia">>),
  {atomic,ok} = wc_mnesia:add_word(<<"politi">>,<<"policia">>),
  Config;

init_per_testcase(_,Config)->
  Config.

end_per_testcase(_Rest,_Config)->
  ok.
%% cases
add_word(_Config) ->
  {atomic,ok} = wc_mnesia:add_word(<<"kat">>,<<"gato">>),
  {atomic,ok} = wc_mnesia:add_word(<<"robot">>,<<"robot">>).

get_all_words(_Config) ->
  Result = wc_mnesia:get_all_words(),
  
  2 = length(Result). %% kat and robot
  
    
find_word_by_name(_Config) ->
  [Word]   = wc_mnesia:find_word(<<"kat">>),
  [Robot]  = wc_mnesia:find_word(<<"robot">>),
  [Rubia]  = wc_mnesia:find_word(<<"blondine">>),
  []       = wc_mnesia:find_word(<<"køre">>),
  [Politi] = wc_mnesia:find_word(<<"politi">>),
    
  <<"kat">>     = Word#wc_word.title,
  <<"dk">>      = Robot#wc_word.language,
  <<"rubia">>   = Rubia#wc_word.definition,
  <<"policia">> = Politi#wc_word.definition.
get_all_words2(_Config) ->    
  Result = wc_mnesia:get_all_words(),
  4 = length(Result).

edit_word(_Config) ->
  {atomic,ok}   = wc_mnesia:edit_word(<<"kat">>,[{definition,<<"gato o gata">>}]),
  [Result]      = wc_mnesia:find_word(<<"kat">>),
  <<"gato o gata">> = Result#wc_word.definition.
  
delete_word(_Config) ->
  {atomic,ok} = wc_mnesia:delete_word(<<"kat">>),
  []       = wc_mnesia:find_word(<<"kat">>),% the word is not found
  [Kat]    = wc_mnesia:find_word_all(<<"kat">>),
  %in this case the word is founded because we as facebook do not
  %delete information but instead we deprecate data,
  %{available=false} 
  false       = Kat#wc_word.available.
  

