%%%-------------------------------------------------------------------
%%% @author oscar_toro <oscar>
%%% @copyright (C) 2015, oscar
%%% @doc
%%% Integration test 
%%% @end
%%% Created : 19 Mar 2015 by oscar <oscar@oscar-K52Dr>
%%%-------------------------------------------------------------------
-module(wc_action_handler_SUITE).
-author("oscar_toro").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/diccionario.hrl").
-define(DEBUG(X),io:format("DEBUG ~p: ~p ~p~n",[?MODULE,?LINE,X])).
%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  wc_mnesia:install(),
  word_collector_app:start(),
  hackney:start(),
  Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->

  ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
  Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
  ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(edit_a_word_api_test, Config) ->
  wc_backend:add_word(<<"bog">>,<<"libro">>),
  wc_backend:add_word(<<"guitar">>,<<"guitarra">>),

  Config;
%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
  Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(delete_a_word_api_test,Config) ->
  wc_mnesia:remove_word(<<"fuld">>),
  wc_mnesia:remove_word(<<"pip">>),
  Config;
end_per_testcase(edit_a_word_api_test,Config) ->
  wc_mnesia:remove_word(<<"bog">>),
  wc_mnesia:remove_word(<<"guitar">>),
  Config;
end_per_testcase(_TestCase, _Config) ->
    ok.


%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [add_a_word_api_test,find_a_word_api_test, delete_a_word_api_test,edit_a_word_api_test].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
%% get_all_words_api_test() -> 
%%     [].

%%--------------------------------------------------------------------
%% @spec add_a_word_api_test(Config0) ->
%%   ok | exit() | {skip,Reason} | {comment,Comment} |
%%   {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% PUT a JSON Word {"title":"pip","definition":"pajarito"}
%% on wc/words 
%% @end
%%--------------------------------------------------------------------
%%CREATE
add_a_word_api_test(Config) -> 
  URL        = <<"localhost:8080/wc/words">>,
  ReqHeaders = [{<<"Content-Type">>,<<"application/json">>}], 
  PayLoad    = <<"{\"word\":{\"title\": \"fuld\",\"definition\": \"curao\"}}">>,

  {ok,_StatusCode,_RespHeaders,ClientRef} = 
    hackney:request(put,URL,ReqHeaders,PayLoad), 
  {ok,Body}  = hackney:body(ClientRef),
  <<"{\"atomic\":\"ok\"}">> = Body,
 
  [Fuld]       = wc_backend:find_word(<<"fuld">>),

  <<"curao">> = Fuld#wc_word.definition,
  Config.


%%--------------------------------------------------------------------
%% @spec find_a_word_test(Config0) ->
%%   ok | exit() | {skip,Reason} | {comment,Comment} |
%%   {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% Find a word (or Retrieve): GET wc/words/word_title
%% @end
%%--------------------------------------------------------------------
%%RETRIEVE
find_a_word_api_test(Config)->

  URL        = <<"localhost:8080/wc/words">>,
  ReqHeaders = [{<<"Content-Type">>,<<"application/json">>}], 
  Payload    = << "{\"word\":{\"title\": \"pip\",\"definition\": \"pajarito\"}}" >>,

  {ok,_StatusCode,_RespHeaders,_ClientRef} = 
    hackney:request(put,URL,ReqHeaders,Payload,[]), 

  URL2     = <<"localhost:8080/wc/words/pip">>,  

  {ok, _StatusCode2, _RespHeaders2,ClientRef2} =
    hackney:request(get,URL2,[], <<>>, []),
  {ok,Body} = hackney:body(ClientRef2),
  Word = wc_json:decode(Body),

  <<"pip">> = Word#wc_word.title,
  Config. 
%%DELETE
delete_a_word_api_test(Config) ->
  
  URL  = <<"localhost:8080/wc/words/fuld">>,
  {ok,_StatusCode,_RespHeaders,ClientRef} =
    hackney:request(delete,URL,[],<<>>,[]),
  {ok,Body} = hackney:body(ClientRef),
  <<"{\"atomic\":\"ok\"}">> = Body,
  [] = wc_backend:find_word(<<"fuld">>),
 % try to add the same word again...
  {ok, _StatusCode2,_RespHeaders2_,ClientRef2} =
    hackney:request(delete,<<"localhost:8080/wc/words/fuld">>,
      [],<<>>,[]),
   {ok,Body2} = hackney:body(ClientRef2),

  <<"{\"atomic\":\"not_found\"}">> = Body2,
  Config.


%%UPDATE
edit_a_word_api_test(Config)->
  URL     = <<"localhost:8080/wc/words/bog">>,
  % URL2    = <<"localhost:8080/wc/words/guitar">>,
  ReqHeader = [{<<"Content-Type">>,<<"application/json">>}],
  PayLoad = <<"{\"word\":\"bog\",\"changes\":{\"definition\":\"libro\",\"status\":\"active\",\"examples\":\"så skal vi læse en bog\"}}"/utf8>>,
  {ok,_StatusCode,_RespHeaders,ClientRef} =
    hackney:request(post,URL,ReqHeader,PayLoad),
  ?DEBUG(_StatusCode),
  ?DEBUG(_RespHeaders),
  {ok,Body} = hackney:body(ClientRef),
  ?DEBUG(Body),
  <<"{\"atomic\":\"ok\"}">> = Body,
  Config.
