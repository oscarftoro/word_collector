%%%-------------------------------------------------------------------
%%% @author oscar
%%% @copyright (CC) 2014, Oscar Toro
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2014 13:47
%%%-------------------------------------------------------------------
-module(wc_mnesia).
-author("oscar_toro").
-include("../include/diccionario.hrl").
-include_lib("stdlib/include/qlc.hrl").
%% API
-export([check_db_exist/1,install/1,install/0]).
-export([add_word/2,get_all_words/0,do/1,find_word/1]).

install(Nodes) ->
        ok = mnesia:create_schema(Nodes),
        rpc:multicall(Nodes, application,start, [mnesia]),
        
        mnesia:create_table(wc_word, 
			    [{type,set},
			     %{record_name, wc_word},
			     {attributes, record_info(fields, wc_word)},
                             {disc_copies, Nodes}]),
        mnesia:create_table(wc_language,
			    [{type,set},
			     %{record_name,wc_language},
			     {attributes, record_info(fields, wc_language)},
                             {disc_copies, Nodes}]),
        rpc:multicall(Nodes,application,stop,[mnesia]).

install() ->
    install([node()]).




check_db_exist(Node)->
        case filelib:wildcard("Mnesia*") of
                [Db_name] ->
                        io:fwrite("Database of name ~p detected ",[Db_name]);
                [] -> install(Node)
        end.



%% add word to the collection
%% T is title whereas D is definition
-spec add_word(binary(),binary()) -> {aborted,string()} | {atomic, string()}.

add_word(T,D) ->
        W = #wc_word{
	      title=T,
	      language="dk",
	      definition=D,
	      status=pasive,
	      priority=medium,
	      examples=undefined,
	      photos=undefined,
	      date_time=calendar:local_time(),
	      available=true},
        F = fun() ->
                mnesia:write(W)
        end,
        case mnesia:transaction(F) of
                {aborted,Reason} -> {aborted,Reason};
                {atomic, Value} -> {atomic, Value}
        end.
-spec get_all_words() -> [#wc_word{}] | [].
get_all_words() ->
        do(qlc:q([X || X <- mnesia:table(wc_word)])).

%% find word by word name
-spec find_word(string()) -> [#wc_word{}] | [].
find_word(W) ->
    wc_mnesia:do(qlc:q([X || X <- mnesia:table(wc_word),X#wc_word.title =:= W])).
    

do(Q) ->
    F = fun()->qlc:e(Q) end,
    {atomic,Val} = mnesia:transaction(F),
    Val.
