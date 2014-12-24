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
-export([check_db_exist/0]).

setup_mnesia() ->
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(word, [{attributes, record_info(fields, word)}]),
        mnesia:create_table(language,[{attributes, record_info(fields, language)}]),
        mnesia:stop().



check_db_exist()->

        case filelib:wildcard("Mnesia*") of
                [Db_name] ->
                        io:fwrite("Database of name ~p detected ",[Db_name]);
                [] -> setup_mnesia()
        end.