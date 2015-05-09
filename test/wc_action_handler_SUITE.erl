%%%-------------------------------------------------------------------
%%% @author oscar_toro <oscar@cph-itlab.dk>
%%% @copyright (C) 2015, oscar
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2015 by oscar <oscar@oscar-K52Dr>
%%%-------------------------------------------------------------------
-module(wc_action_handler_SUITE).
-author("oscar_toro").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/diccionario.hrl").
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
  word_collector_app:stop(),
  hackney:stop(),
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
    [add_a_word_api_test].

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
add_a_word_api_test(Config) -> 
  Method     = put,
  URL        = <<"localhost:8080/wc/words">>,
  ReqHeaders = [{<<"Content-Type">>,<<"application/json">>}], 
  Payload    = << "{\"title\": \"pip\",\"definition\": \"pajarito\"}" >>,

  {ok,_StatusCode,_RespHeaders,ClientRef} = 
    hackney:request(Method,URL,ReqHeaders,Payload,[]),
  
  {ok,Body}  = hackney:body(ClientRef),

  <<"{\"atomic\":\"ok\"}">> = Body,
  [Pip]      = wc_backend:find_word("pip"),
  "pajarito" = Pip#wc_word.title,
  Config.


   
    
