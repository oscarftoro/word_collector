%%%-------------------------------------------------------------------
%%% @author oscar toro
%%% @copyright (CC) 2015,  Oscar Toro
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
-export([ create_resource/2 
        % is_conflict/2,
        % resource_exists/2
       ]).
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

%% for GET 
content_types_provided(Req, State) ->
 
 {[{<<"application/json">>, handle_request}], Req, State}.

%% for POST and PUT
content_types_accepted(Req, State) ->
  {[{<<"application/json">>, create_resource}],Req, State}.

%% resource_exists(Req, State) ->
%%   {false,Req,State}.

%% is_conflict(Req,State) ->
%%   {false,Req,State}.

handle_request(Req,State) ->
  handle_method(cowboy_req:get(method,Req),Req,State).

create_resource(Req,State) ->
  create_or_edit(cowboy_req:get(method,Req),Req,State).

%% to retrieve a word or words we use GET
-spec handle_method(<<_:24,_:_*8>>,_,_)->{<<_:64,_:_*8>>,_,_}.
handle_method(<<"GET">>,Req,State)->
  {AllBindings,_Req1} = cowboy_req:bindings(Req),
  Words = get_resource(AllBindings),
  ?DEBUG(Words),
  {Words,Req,State}.

%% 
%%To create a word we use PUT
create_or_edit(<<"PUT">>,Req,State) ->
  {ok,Body,_Req0} = cowboy_req:body(Req),% get the Body

  case  cowboy_req:bindings(Req) of
    {[{what,<<"words">>}],_Req1}->
      create_res(Body,Req,State);
    {[{what,<<"languages">>}],_Req1}->
      create_res(Body,Req,State)
  end, 
 
  {<<"hi">>,Req,State};
%%
%% To edit a word we use POST
create_or_edit(<<"POST">>,Req,State) ->
   {AllBindings,_Req1} = cowboy_req:bindings(Req),
   ?DEBUG(AllBindings),AllBindings,  
   {AllValues,_Req2}   = cowboy_req:qs_vals(Req),
   ?DEBUG(AllValues),AllValues,
   {<<"a POST">>,Req,State}.

%%%%%%%%%%%%%%%%%%%%
%%%REST RESOURCES%%%
%%%%%%%%%%%%%%%%%%%%
              
get_resource([{what,<<"words">>}]) ->
  Words = wc_backend:get_all_words(),
  Result = wc_json:encode(Words),
  Result;
 
get_resource([what,<<"languages">>]) ->
    ok;

get_resource([{item,Word},{what,<<"words">>}])->
  ?DEBUG(Word),
  LWords  = wc_backend:find_word(Word),
  Result = encode_list(LWords),
  ?DEBUG(Result),  
  Result;
		  
get_resource([{item,ItemName},{what,<<"language">>}])->
  ItemName,ok.		     
		      
create_res(Body,Req,State)->
  {Type,PList} = wc_json:decode_from_web(Body),
  create(Type,PList,Req,State).

create(<<"word">>,PList,Req,State)->
  [Title,Definition] = 
    [proplists:get_value(K,PList) || {K,_V} <- PList],
  Result = wc_backend:add_word(Title,Definition),
  Resp = jiffy:encode(Result),
  {ok, Req2} = 
    cowboy_req:reply(201,[{<<"server">>,<<"Apache">>}],Resp,Req),
  
  {Resp, Req2, State};
      
create(<<"language">>,_PList,_Req,_State) ->
    unimplemented. 

%%%%%%%%%%%%%%%%%%%%%%
%%auxiliar functions%%
%%%%%%%%%%%%%%%%%%%%%%
 
%% -spec word_exists(binary())-> boolean().
%% word_exists(WordName)->
%%   ok.
    
%% check whether the result is a list of several elements
%% otherwise is going to encode the resource as one element
%% 
-spec encode_list([#wc_word{} | #wc_language{}])->binary().
encode_list(List) ->
  case length(List) of
    1 ->  
       [L] = List,
	  wc_json:encode(L);
    0 ->
	  erlang:error("no words to encode. Error at line 146");  
    _ -> wc_json:encode(List)
  end.
