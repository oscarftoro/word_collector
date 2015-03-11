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

-include_lib("eunit/include/eunit.hrl").
%% API
-export([init/3]).
-export([content_types_provided/2,
	allowed_methods/2]).
-export([handle_request/2]).
-include("../include/diccionario.hrl").

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req,State)->
  {[<<"GET">>,<<"POST">>,<<"DELETE">>],Req,State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_request}], Req, State}.

handle_request(Req,State)->
  handle_method(cowboy_req:method(Req),Req,State).

-spec handle_method(<<_:24,_:_*8>>,_,_)->{<<_:64,_:_*8>>,_,_}.
handle_method(<<"GET">>,Req,State)->
  {<<"{\"rest\": \"Hello World!\"}">>,Req,State};
handle_method(<<"POST">>,Req,State) ->
  {<<"That was a POST request">>,Req,State};
handle_method(<<"DELETE">>,Req,State)->
  {<<"That was a DELETE method">>,Req,State}.

create_word_test()->
  ok.
