-module(word_collector_app).
-behaviour(application).

-export([start/0,start/2]).
-export([stop/1,stop/0]).
-define(APP,word_collector).

start(_Type, _Args) ->
  ok = wc_mnesia:check_db_exist([node()]),
  mnesia:wait_for_tables([wc_word,wc_language],5000),

  Path_list = path_list(), 
  Dispatch = cowboy_router:compile([
    {'_',Path_list}
  ]),
    
  {ok,_pid} = cowboy:start_http(word_collector_app,10,[{port,8080}],
  [{env,[{dispatch,Dispatch}]}]),

  word_collector_sup:start_link().

stop(_State) ->
	ok.

path_list() ->

  Static_assets = {"/[...]",cowboy_static,{priv_dir,word_collector,"client"}},
  Index         = {"/",cowboy_static,{priv_file,word_collector,"client/index.html",
  [{mimetypes, {<<"text">>, <<"html">>, []}}]}},
  Action        =  {"/wc/:what/[:item]",wc_action_handler,[]},%for CREATE,READ_ALL, Search
  [Action,Index,Static_assets].

start() ->
  application:ensure_all_started(word_collector).

stop() ->
  application:stop(word_collector).

