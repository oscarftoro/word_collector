-module(wc_action_handler_SUITE).
-author("oscar_toro").
-include_lib("common_test/include/ct.hrl").
-include("../include/diccionario.hrl").

all()->
    [handle_method_test].
init_per_suite(_Config)->
    word_collector_app:start(),
    ok.
end_per_suite(_Config) ->
    word_collector_app:stop(),
    ok.
init_per_testcase(handle_req_test,Config)->
    {ok,Config}.
