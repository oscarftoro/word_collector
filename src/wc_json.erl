-module(wc_json).
-export([json_to_record/1]).
-include_lib("eunit/include/eunit.hrl").
-include("../include/diccionario.hrl").
-include_lib("../deps/jsonrec/include/jsonrec.hrl").

json_to_record(Json)->
  {ok,Record} = word:from_json(Json),
  Record.

json_to_record_test()->
  W = #wc_word{title="numse", definition="popin"},
  W = json_to_record(<<"{\"word\":{\"title\": \"numse\",
  \"definition\":\"popin\"}}">>).

record_to_json_test() ->
    W = #wc_word{title="numse", definition="popin"},
    
