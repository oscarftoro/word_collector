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
	 allowed_methods/2,
         content_types_accepted/2]).
-export([handle_request/2]).
-export([create_resource/2]).
-include("../include/diccionario.hrl").

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

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_resource}],Req, State}.

handle_request(Req,State) ->
  handle_method(cowboy_req:get(method,Req),Req,State).

create_resource(Req,State) ->
  create_or_edit(cowboy_req:get(method,Req),Req,State).

-spec handle_method(<<_:24,_:_*8>>,_,_)->{<<_:64,_:_*8>>,_,_}.
handle_method(<<"GET">>,Req,State)->
  {AllBindings,_Req1} = cowboy_req:bindings(Req),
  ?DEBUG(AllBindings),
   
  Words = get_resource(AllBindings),
  ?DEBUG(Words),
  {Words,Req,State}.

%% To create a new Word
%%
create_or_edit(<<"PUT">>,Req,State) ->
  {AllBindings,_Req1} = cowboy_req:bindings(Req),
  ?DEBUG(AllBindings),
  {AllValues,Req2}   = cowboy_req:qs_vals(Req),
  ?DEBUG(AllValues),
  ?DEBUG(Req2),
  Result = handle_put(AllBindings,Req),
  {Result,Req,State};

create_or_edit(<<"POST">>,Req,State) ->
   {AllBindings,_Req1} = cowboy_req:bindings(Req),
   ?DEBUG(AllBindings),  
   {AllValues,_Req2}   = cowboy_req:qs_vals(Req),
   ?DEBUG(AllValues),
   {<<"That was a POST">>,Req,State}.

    
handle_put([{first,What}],Req) when What =:= <<"word">> ->
  {Word_title,_Req} = cowboy_req:qs_val(<<"title">>,Req), 
  ?DEBUG(Word_title),
  {Word_desc,_Req1} = cowboy_req:qs_val(<<"definition">>,Req),
  ?DEBUG(Word_desc),
  {ok,atomic} = wc_backend:add_word(Word_title,Word_desc),
  <<"{\"result\": \"ok\"}">>.

%%%%%%%%%%%%%%%%%%%%
%%%REST RESOURCES%%%
%%%%%%%%%%%%%%%%%%%%
              
get_resource([{what,<<"words">>}]) ->
  Words = wc_backend:get_all_words(),

  Result = wc_json:encode(Words),
  Result;
 
get_resource([what,<<"languages">>]) ->
    ok;

get_resource([{item,ItemName},{what,<<"words">>}]) ->
  ItemName,ok;
		  
get_resource([{item,ItemName},{what,<<"language">>}])->
  ItemName,ok.		     
		  
    
