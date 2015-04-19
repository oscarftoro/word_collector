%%%-------------------------------------------------------------------
%%% @author oscar toro
%%% @copyright (CC) 2014,  Oscar Toro
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2014 13:58
%%%-------------------------------------------------------------------
-module(wc_action_handler).
-author("oscar").

%% API
-export([init/3]).
-export([content_types_provided/2,
	allowed_methods/2]).
-export([handle_request/2]).
-include("../include/diccionario.hrl").
%% define a debug macro
%% c(wc_action_handler,{d,debug_flag}).
-ifdef(debug_flag).
-define(DEBUG(X),io:format("DEBUG ~p: ~p ~p~n",[?MODULE,?LINE,X])).
-else.
-define(DEBUG(X),void).
-endif.

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req,State)->
  {[<<"GET">>,<<"PUT">>,<<"POST">>,<<"DELETE">>],Req,State}.

%% for GET PUT POST and DELETE
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_request}], Req, State}.

handle_request(Req,State)->
  handle_method(cowboy_req:method(Req),Req,State).

-spec handle_method(<<_:24,_:_*8>>,_,_)->{<<_:64,_:_*8>>,_,_}.
handle_method(<<"GET">>,Req,State)->
  {<<"{\"rest\": \"Hello World!\"}">>,Req,State};
%to create a new Word
handle_method(<<"PUT">>,Req,State) ->
  {AllBindings,_Req1} = cowboy_req:bindings(Req),
  ?DEBUG(AllBindings),
  Result = handle_put(AllBindings,Req),
  {Result,Req,State};

handle_method(<<"DELETE">>,Req,State)->
  {<<"That was a DELETE method">>,Req,State}.

%% when creating resource is a word
handle_put([{first,What}],Req) when What =:= <<"word">> ->
  {Word_title,_Req} = cowboy_req:qs_val(<<"title">>,Req), 
  ?DEBUG(Word_title),
  {Word_desc,_Req1} = cowboy_req:qs_val(<<"definition">>,Req),
  ?DEBUG(Word_desc),
  {ok,atomic} = wc_backend:add_word(Word_title,Word_desc),
  <<"{\"result\": \"ok\"}">>.
    
