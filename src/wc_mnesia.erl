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
-export([add_word/2,get_all_words/0,do/1,find_word/1,   
         edit_word/2,delete_word/1,get_deleted_words/0,
         remove_word/1]).
% exported only for tests purppose, 
% delete when we are done with db implementation
-export([add_word/1]).

-spec install(atom() | [atom()]) -> {[any()],[atom()]}.
install(Nodes) ->
  ok                 = mnesia:create_schema(Nodes),
  {[_Any],_AtomList} = rpc:multicall(Nodes, application,start, [mnesia]),
        
  mnesia:create_table(wc_word, 
    [{type,set},
    {attributes, record_info(fields, wc_word)},
    {disc_copies, Nodes}]),

  mnesia:create_table(wc_language,
    [{type,set},
    {attributes, record_info(fields, wc_language)},
    {disc_copies, Nodes}]),
    rpc:multicall(Nodes,application,stop,[mnesia]).

-spec install() -> {[any()],[atom()]}.
install() ->
  install([node()]).

-spec check_db_exist(atom()| [atom()])-> ok | {[any()],[atom()]}.
check_db_exist(Node)->
  case filelib:wildcard("Mnesia*") of
    [Db_name] ->
      io:fwrite("Database of name ~p detected ",[Db_name]), ok;
    [] -> install(Node)
      end.

%% add word to the collection
%% T is title whereas D is definition
-spec add_word(undefined | binary(),undefined | binary()) -> {'aborted',_} | {'atomic',_ }.

add_word(T,D) ->
  W = #wc_word{title=T,definition=D},
  add_word(W).

-spec add_word(#wc_word{}) -> {'aborted',_} | {'atomic',_}.
add_word(W) ->
  F = fun() ->
        mnesia:write(W)
      end,
  case mnesia:transaction(F) of
    {aborted,Reason} -> {aborted,Reason};
    {atomic, Value} -> {atomic, Value}
  end.
    
-spec get_all_words() -> [#wc_word{}] | [].
get_all_words() ->
  do(qlc:q([X || X <- mnesia:table(wc_word),X#wc_word.available =:= true])).

-spec get_deleted_words() -> [#wc_word{}] | [].
get_deleted_words() ->
  do(qlc:q([X || X <- mnesia:table(wc_word),X#wc_word.available =:= false])).
%% find word by word name
%% remember, you get a list of results!!!
-spec find_word(binary()) -> [#wc_word{}] | [].
find_word(W) ->
  wc_mnesia:do(qlc:q([X || X <- mnesia:table(wc_word),X#wc_word.title =:= W])).

%% 
%% first parameter is the name(binary()) of the word to be edited, 
%% second parameter is a list of tuples containing
%% changes. For example: 
%% [{status,<<"pasive">>},{definition,<<"new definition">>},
%% {priority,<<"high">>}]
   
-spec edit_word(binary(),[{atom(),binary() | boolean() |
 [] | [binary()],[integer()]}])-> {'aborted',_} | {'atomic',_}.
edit_word(W,PropList)->
  [Word] = find_word(W),
  NewW =edit_word_helper(Word,PropList),
  add_word(NewW).

%% dinamically edit the record to be updated
edit_word_helper(Word,PropList)->
  lists:foldr(fun({Index,NewValue},Acc)-> 
    setelement(translate_index(Index),Acc,NewValue)    
  end,Word,PropList).

-spec translate_index(atom()) -> integer().

translate_index(title)      -> 2;
translate_index(language)   -> 3;
translate_index(definition) -> 4;
translate_index(status)     -> 5;
translate_index(priority)   -> 6;
translate_index(examples)   -> 7;
translate_index(locations)  -> 8;
translate_index(photos)     -> 9;
translate_index(date_time)  -> 10;
translate_index(available)  -> 11.

-spec delete_word(binary()) ->  no_return().
delete_word(WordName)->
  edit_word(WordName,[{available,false}]).

remove_word(WordName)->
  Oid = {wc_word,WordName},
  F = fun() ->
    mnesia:delete(Oid)
  end,
    mnesia:transaction(F).

%% Evaluate qlc queries of type:
%% qlc:q(query()).
%% query() = [Something || Something <- mnesia:table(SomeTable),Predicate]
do(Q) ->
  F = fun()->qlc:e(Q) end,
  {atomic,Val} = mnesia:transaction(F),
  Val.
