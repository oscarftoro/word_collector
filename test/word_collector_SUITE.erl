-module(word_collector_SUITE).
-author("oscar_toro").
-include_lib("common_test/include/ct.hrl").
-include("../include/diccionario.hrl").
-export([init_per_suite/1,init_per_testcase/2,end_per_testcase/2,
	 all/0,add_word/1,
	 get_all_words/1,word_by_name/1, edit_word/1]).

all() ->
    [add_word,get_all_words,word_by_name,edit_word].

init_per_suite(Config) ->
   
    wc_mnesia:install([node()]),
    word_collector_app:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

init_per_testcase(get_all_words,Config)->
    {atomic,ok} = wc_mnesia:add_word("hej","hola"),
    {atomic,ok} = wc_mnesia:add_word("hej hej", "chao"),
    Config;
init_per_testcase(word_by_name,Config) ->
    {atomic,ok} = wc_mnesia:add_word("blondine","rubia"),
    {atomic,ok} = wc_mnesia:add_word("politi", "policia"),
    Config;
init_per_testcase(_,Config) ->
    Config.

end_per_testcase(_,_Config)->
    ok.
%% cases
add_word(_Config) ->
    {atomic,ok} = wc_mnesia:add_word("kat","gato"),
    {atomic,ok} = wc_mnesia:add_word("robot","robot").

get_all_words(_Config) ->
   [One,Two,Three,Four] = wc_mnesia:get_all_words().
    
word_by_name(_Config) ->
    [Word]  = wc_mnesia:find_word("kat"),
    [Robot] = wc_mnesia:find_word("robot"),
    [Rubia] = wc_mnesia:find_word("blondine"),
    []      = wc_mnesia:find_word("køre"),
    
    Word#wc_word.title       =:= "kat",
    Robot#wc_word.language   =:= "dk",
    Rubia#wc_word.definition =:= "rubia".
edit_word(_Config) ->
    [Kat] = wc_mnesia:find_word("kat"),
    {atomic,ok} = wc_mnesia:edit_word(Kat,definition,"gato o gata"),
    [Result] = wc_mnesia:find_word("kat"),
    Result#wc_word.definition =:= "gato o gata".
