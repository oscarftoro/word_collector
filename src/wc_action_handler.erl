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
-export([ init/3]).
-export([ content_types_provided/2,
	  allowed_methods/2,
          content_types_accepted/2]).
-export([ handle_request/2]).
-export([create_resource/2, 
         is_conflict/2,
         resource_exists/2]).
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
  {[{<<"application/json">>, create_resource}],Req, State}.

resource_exists(Req, State) ->
    {false,Req,State}.
is_conflict(Req,State) ->
    {false,Req,State}.

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

%% 
%%To create a word we use PUT
create_or_edit(<<"PUT">>,Req,State) ->
  {ok,Body,_Req0} = cowboy_req:body(Req),% get the Body

  case  cowboy_req:bindings(Req) of
    {[{what,<<"words">>}],_Req1}->
      create_resource(Body,Req,State);
    {[{what,<<"languages">>}],_Req1}->
      unimplemented %edit_word(Body,Req,State)
  end, 
 
  {<<"hi">>,Req,State};
%%
%% To edit a word we use POST
create_or_edit(<<"POST">>,Req,State) ->
   {AllBindings,_Req1} = cowboy_req:bindings(Req),
   ?DEBUG(AllBindings),  
   {AllValues,_Req2}   = cowboy_req:qs_vals(Req),
   ?DEBUG(AllValues),
   {<<"That was a POST">>,Req,State}.



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
		  
    
create_resource(Body,Req,State)->
  {Type,PList} = wc_json:decode_from_web(Body),
  create(Type,PList,Req,State).

create(<<"word">>,PList,Req,State)->
  [Title,Definition] = 
    [proplists:get_value(K,PList) || {K,_V} <- PList],
  Result = wc_backend:add_word(Title,Definition),
  Resp = jiffy:encode(Result),
  {ok, Req2} = 
    cowboy_req:reply(201,[{<<"server">>,<<"Apache">>}],Resp,Req),
  
  ?DEBUG(Resp),
  ?DEBUG(Req2),
  {Resp, Req2, State};
      
create(<<"language">>,_PList,_Req,_State) ->
    unimplemented. 
