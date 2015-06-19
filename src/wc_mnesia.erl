%%%-------------------------------------------------------------------
%%% @author oscar
%%% @copyright (CC) 2015, Oscar Toro
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
         recover_word/1,find_word_all/1,
         remove_word/1]).
% exported only for tests purppose, 
% TODO: delete when we are done with db implementation
-export([add_word/1]).
%%--------------------------------------------------------------
%% @doc
%% Confirm that mnesia schema is available. If not, it creates the required tables wc_word and wc_language
%% 
%% @end
%%--------------------------------------------------------------
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
%%--------------------------------------------------------------
%% @doc
%% Call install with the current node as a parameter.
%% 
%% @end
%%--------------------------------------------------------------
-spec install() -> {[any()],[atom()]}.
install() ->
  install([node()]).
%%--------------------------------------------------------------
%% @doc
%% Auxiliar function that do what it says to do.
%% 
%% @end
%%--------------------------------------------------------------
-spec check_db_exist(atom()| [atom()])-> ok | {[any()],[atom()]}.
check_db_exist(Node)->
  case filelib:wildcard("Mnesia*") of
    [Db_name] ->
      io:fwrite("Database of name ~p detected ",[Db_name]), ok;
    [] -> install(Node)
      end.

%%--------------------------------------------------------------
%% @doc
%% add word to the collection
%% T is title whereas D is definition. Both has to be binaries
%% @end
%%--------------------------------------------------------------

-spec add_word(undefined | binary(),undefined | binary()) -> {'aborted',_} | {'atomic',_ }.

add_word(T,D) ->
  W = #wc_word{title=T,definition=D},
  add_word(W).
%%--------------------------------------------------------------
%% @doc
%% Add word to the collection. Take a record of type wc_word
%% 
%% @end
%%--------------------------------------------------------------
-spec add_word(#wc_word{}) -> {'aborted',_} | {'atomic',_}.
add_word(W) ->
  F = fun() ->
        mnesia:write(W)
      end,
  case mnesia:transaction(F) of
    {aborted,Reason} -> {aborted,Reason};
    {atomic, Value} -> {atomic, Value}
  end.
%%--------------------------------------------------------------
%% @doc
%% As you would expect, this function will retrieve all the words from the database
%% 
%% @end
%%--------------------------------------------------------------    
-spec get_all_words() -> [#wc_word{}] | [].
get_all_words() ->
  do(qlc:q([X || X <- mnesia:table(wc_word),X#wc_word.available =:= true])).
%%--------------------------------------------------------------
%% @doc
%% Get a list of words with the value available = false
%% 
%% @end
%%-------------------------------------------------------------- 
-spec get_deleted_words() -> [#wc_word{}] | [].
get_deleted_words() ->
  do(qlc:q([X || X <- mnesia:table(wc_word),X#wc_word.available =:= false])).
%%--------------------------------------------------------------
%% @doc
%% Find word by word name. The search is by name and has to be a binary.
%% remember, you get a list of results!!!
%% @end
%%-------------------------------------------------------------- 

-spec find_word(binary()) -> [#wc_word{}] | [].
find_word(W) ->
  wc_mnesia:do(qlc:q([X || X <- mnesia:table(wc_word),X#wc_word.title =:= W, X#wc_word.available =:= true])).
%%--------------------------------------------------------------
%% @doc
%% Find word by name, including deleted words.
%% @end
%%-------------------------------------------------------------- 

find_word_all(W)->
   wc_mnesia:do(qlc:q([X || X <- mnesia:table(wc_word),X#wc_word.title =:= W])).    

%%--------------------------------------------------------------
%% @doc
%% First parameter is the name(binary()) of the word to be edited, 
%% Second parameter is a list of tuples containing
%% changes. For example: 
%% [{status,passive::binary()},{definition,new definition::binary()},
%% {priority,high::binary()}]
%% @end
%%-------------------------------------------------------------- 

   
-spec edit_word(binary(),[{atom(),binary() | boolean() |
 [] | [binary()],[integer()]}])-> {'aborted',_} | {'atomic',_} | [].

edit_word(W,[{available,false}]) ->
  case find_word(W) of
    [_Word] -> do_edit_word(W,[{available,false}]);
    []      -> []
  end;

edit_word(W,PropList)->
  do_edit_word(W,PropList).

do_edit_word(W,PropList)->
  case find_word(W) of
    [] -> {atomic,not_found};
    [Word] -> NewW =edit_word_helper(Word,PropList),
              add_word(NewW) %{atomic,ok}
  end.


%%--------------------------------------------------------------
%% @doc
%% recover deleted word
%% to check deleted words call get_deleted_words()
%% @end
%%--------------------------------------------------------------
recover_word(W)->
  [Word]  = find_word_all(W),
  RecWord = edit_word_helper(Word,[{available,true}]), 
  add_word(RecWord).

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
%%--------------------------------------------------------------
%% @doc
%% Set value of word available to false. This function does not delete words, but instead hide them.
%% 
%% @end
%%--------------------------------------------------------------
-spec delete_word(binary()) ->  no_return().
delete_word(WordName)->
  edit_word(WordName,[{available,false}]).
%%--------------------------------------------------------------
%% @doc
%% This function really delete words from the database. Use with caution
%% 
%% @end
%%--------------------------------------------------------------
remove_word(WordName)->
  Oid = {wc_word,WordName},
  F = fun() ->
    mnesia:delete(Oid)
  end,
    mnesia:transaction(F).
%%--------------------------------------------------------------
%% @doc
%% Evaluate qlc queries of type:
%% qlc:q(query()).
%% @end
%%--------------------------------------------------------------
%% Evaluate qlc queries of type:
%% qlc:q(query()).
%% query() = [Something || Something <- mnesia:table(SomeTable),Predicate]
do(Q) ->
  F = fun()->qlc:e(Q) end,
  {atomic,Val} = mnesia:transaction(F),
  Val.
