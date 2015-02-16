%%%-------------------------------------------------------------------
%%% @author oscar
%%% @copyright (CC) 2014, Oscar Toro
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2014 13:47
%%%-------------------------------------------------------------------
-module(wc_mnesia).
-author("oscar").
-include("../include/diccionario.hrl").

%% API
-export([check_db_exist/0, add_word/1]).

setup_mnesia() ->
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(word, [{type,set},{record_name, word},{attributes, record_info(fields, word)}]),
        mnesia:create_table(language,[{type,set},{record_name,language},{attributes, record_info(fields, language)}]),
        mnesia:stop().



check_db_exist()->
        case filelib:wildcard("Mnesia*") of
                [Db_name] ->
                        io:fwrite("Database of name ~p detected ",[Db_name]);
                [] -> setup_mnesia()
        end.



-spec add_word(#word{}) -> {aborted,string()} | {atomic, string()}.

add_word(W) ->

        F = fun() ->
                mnesia:write(W)
        end,
        case mnesia:transaction(F) of
                {aborted,Reason} -> {aborted,Reason};
                {atomic, Value} -> {atomic, Value}
        end.



