%%%-------------------------------------------------------------------
%%% @author oscar toro
%%% @copyright (CC) 2015,  Oscar Toro
%%% @doc
%%% Action handler is where the magic of the REST handling happens.
%%% Cowboy is using this module to deal with the requests
%%% @end
%%% Created : 14. Nov 2014 13:58
%%%-------------------------------------------------------------------
-module(wc_action_handler).
-author("oscar_toro").

%% API
-export([ init/3]).
-export([ content_types_provided/2,
          options/2,
	  allowed_methods/2,
          content_types_accepted/2]).
-export([ handle_request/2]).
-export([ create_resource/2, 
          delete_resource/2,
          delete_completed/2
         
       ]).
-include("../include/diccionario.hrl").
%% a Macro to debug 
-ifdef(debug_flag).
-define(DEBUG(X),io:format("DEBUG ~p: ~p ~p~n",[?MODULE,?LINE,X])).
-else.
-define(DEBUG(X),void).
-endif.

%%-----------------------------------------------------------------
%% @doc
%% Initialization Callback. This is the first callback that is called. Yes, as an in a gen_fsm or gen_fsm. This callback performs an upgrade to allow the use of REST for the current request. 
%% After excution, Cowboy switch to the REST protocol, and behaves as a state machine.
%% @end
%%-----------------------------------------------------------------

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.
%%--------------------------------------------------------------
%% @doc
%% Define allowed HTTP methods. In our case we are going to use PUT,GET,POST and DELETE to allow CRUD actions and OPTIONS to allow CORS
%% @end
%%--------------------------------------------------------------
allowed_methods(Req,State)->
  {[<<"PUT">>,<<"GET">>,<<"POST">>,<<"DELETE">>,<<"OPTIONS">>],Req,State}.
%%--------------------------------------------------------------
%% @doc
%% In order to allow CORS we have to implement the following callback and furthermore give the correct response into the header.
%%More info at www.google.dk/#q=cors or at https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS
%% @end
%%--------------------------------------------------------------
options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"PUT,GET,POST,DELETE,OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req1),
   
 Req3 =  
  cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>,
  <<"Content-Type">>,Req2),
    ?DEBUG(Req3),
    {ok, Req3, State}.

%%--------------------------------------------------------------
%% @doc
%% Every time a resource is request, this callback is called.
%% Therefore content_types_provided callback is necesary to be defined in order to distinguish the content-type that the web service is going to serve. We are specifying that we are providing responses in JSON format.
%% @end
%%--------------------------------------------------------------
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_request}],Req,State}.
 
%%--------------------------------------------------------------
%% @doc
%% In order to support POST and PUT methods, this call has to be defined. Both PUT and POST reach the content_type_accepted step. We are also accepting JSON format so this is the type we are going to enable.
%% when one of those methods are called these are going to be handled by create_resource function. I couldn't find a better name at the moment
%% @todo change the name of the function handler for POST and PUT
%% @end
%%--------------------------------------------------------------

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, create_resource}],Req,State}.

%% for DELETE we have to implement the delete_resource callback
delete_resource(Req,_State) ->
  {Item,Req2} = cowboy_req:binding(item,Req),  
 
  case wc_backend:delete_word(Item) of
    {[[]]}          -> {true,Req2,{[{atomic,not_found}]}};
    {[{atomic,ok}]} -> {true,Req2,{[{atomic,ok}]}}
  end.% pass the result as state

%% return 200 when the word is deleted
%% and 404 when the resource is not found
delete_completed(Req,State) ->   
  Response = jiffy:encode(State),% <<"{\"atomic\":\"ok\"}">>
  case State of
    {[{atomic,ok}]} ->
      {ok, Req2} =       
        cowboy_req:reply(200,[{<<"server">>,<<"Apache">>}],
        Response,Req),
      {true,Req2,State};
    {[{atomic,not_found}]}->
      {ok, Req2} =       
        cowboy_req:reply(404,[{<<"server">>,<<"Apache">>}],
        Response,Req),
      {false,Req2,State}
  end.
       

handle_request(Req,State) ->
  handle_method(cowboy_req:get(method,Req),Req,State).

create_resource(Req,State) ->
  create_or_edit(cowboy_req:get(method,Req),Req,State).

%% to retrieve a word or words we use GET
-spec handle_method(<<_:24,_:_*8>>,_,_)->{<<_:64,_:_*8>>,_,_}.
handle_method(<<"GET">>,Req,State)->
  {AllBindings,_Req1} = cowboy_req:bindings(Req),
  Words = get_resource(AllBindings),
   Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),%%ONLY FOR A MOMENTARY SOLUTION!
 

  {Words,Req2,State}.

%% 
%%To create a word we use PUT
create_or_edit(<<"PUT">>,Req,State) ->
  ?DEBUG(Req),
  {ok,Body,_Req0} = cowboy_req:body(Req),% get the Body
  case  cowboy_req:bindings(Req) of
    {[{what,<<"words">>}],_Req1}->
      create_res(Body,Req,State);
    {[{what,<<"languages">>}],_Req1}->
      create_res(Body,Req,State)
  end;%TODO:DELETE THIS RETURN
%%
%% To edit a word we use POST
create_or_edit(<<"POST">>,Req,State) -> 
  {ok,Body,_Req} = cowboy_req:body(Req),
 
  Result = edit_resource(jiffy:decode(Body),Req,State),
  ?DEBUG(Result),
  Result.


%%%%%%%%%%%%%%%%%%%%
%%%REST RESOURCES%%%
%%%%%%%%%%%%%%%%%%%%
      
% GET ALL WORDS        
get_resource([{what,<<"words">>}]) ->
  Words = wc_backend:get_all_words(),
  Result = wc_json:encode(Words),
  Result;
 
get_resource([what,<<"languages">>]) ->
    ok;

get_resource([{item,Word},{what,<<"words">>}])->
  LWords  = wc_backend:find_word(Word),
  Result = encode_list(LWords),  
  Result;
		  
get_resource([{item,ItemName},{what,<<"language">>}])->
  ItemName,ok.		     
		      
create_res(Body,Req,State)->
  Record = wc_json:decode(Body),%decode JSON to Erlang Resource Record
  Type = element(1,Record), %which kind of record?
  create(Type,Record,Req,State).       % now creating a real word
%%PUT WORD
create(wc_word,Word,Req,State)->
 %%TODO: iterate over the values and generate a word
  Result =
    wc_backend:add_word(Word),
  Resp = jiffy:encode(Result),

  ?DEBUG(Resp),
  {ok, Req2} = 
    cowboy_req:reply(201,[{<<"server">>,<<"Apache">>},{ 
    <<"access-control-allow-origin">>,
    <<"*">>}],Resp,Req),
  
  {Resp, Req2, State};
      
create(<<"language">>,_PList,_Req,_State) ->
    unimplemented. 
    
edit_resource({[{<<"word">>,Item},{_Changes,{PropList}}]},Req,State) ->
  %the key has to be an atom not a binary
  PL = [{binary_to_atom(K,utf8),V}|| {K,V} <- PropList],

  Result = wc_backend:edit_word(Item,PL),
  Resp = jiffy:encode(Result),
  case Result of
    {[{atomic,ok}]} -> 
      {ok, Req2} = 
      cowboy_req:reply(200,[{<<"server">>,<<"Apache">>},
      {<<"Access-Control-Allow-Origin">>,<<"*">>}],Resp,Req),  
      {jiffy:encode({[{atomic,ok}]}), Req2, State};

    {[{atomic,not_found}]} ->
      {ok, Req2} =
      cowboy_req:reply(404,[{<<"server">>,<<"Apache">>},
      {<<"Access-Control-Allow-Origin">>,<<"*">>}],Resp,Req),
      {jiffy:encode({[{atomic,not_found}]}), Req2, State}
  end;

edit_resource({[{<<"language">>,Item},{_Changes,{PropList}}]},Req,State) ->
  Item,PropList,
  {<<"language">>,Req,State}.
    


%%%%%%%%%%%%%%%%%%%%%%
%%auxiliar functions%%
%%%%%%%%%%%%%%%%%%%%%%
     
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
	  erlang:error("no words to encode. Error at encode_list @ wc_action_handler.Are you sure you entered a word and not a space or an invalid character?");  
    _ -> wc_json:encode(List)
  end.


