-module(wc_json).
-export([encode/1,decode/1,record_to_proplist/1]).


-include_lib("eunit/include/eunit.hrl").
-include("../include/diccionario.hrl").

-spec encode(#wc_word{}) -> binary().
encode(#wc_word{} = Rec) ->  
  jiffy:encode({record_to_proplist(Rec)});
encode(#wc_language{} = Rec) ->
  jiffy:encode({record_to_proplist(Rec)}).

%% TODO: Implement languages also
%% decode only words
decode(Bin) ->
  {PrpList} = jiffy:decode(Bin),
  PropList  = [{binary_to_atom(K,utf8),V} || {K,V} <- PrpList],
  case hd(PropList) of
    {title,_Value} -> decoder(PropList,lists:seq(2,11));
    {name, _Value}  -> decoder(PropList,lists:seq(2,4))
  end.
	  
decoder(PL,L) ->

  Values    = [V ||  {_K,V} <- PL], %% all the values
  Formated  = lists:zip(L,Values), %proplist{int,binary() | []}
  Word      = #wc_word{},
  lists:foldr(fun({K,V},Acc) -> setelement(K,Acc,V) end,Word,Formated).   

    
-spec record_to_proplist(#wc_word{}) -> [{atom(),any()}].
record_to_proplist(#wc_word{} = Rec) ->
  lists:zip(record_info(fields,wc_word),   
  tl(tuple_to_list(Rec)));

record_to_proplist(#wc_language{} = Rec) ->
  lists:zip(record_info(fields,wc_language),
  tl(tuple_to_list(Rec))).

encode_test() ->

  W = #wc_word{title = <<"numse">>, definition = <<"popin">>},
  Encoded = encode(W),
  W2 = decode(Encoded),
  ?assert(<<"numse">> =:= W2#wc_word.title),     
  ?assert(<<"popin">> =:= W2#wc_word.definition).
    

 
%% decode_test()->
%%   W = #wc_word{title="numse", definition="popin"},
%%   ok = decode(<<"{\"word\":{\"title\": \"numse\",
%%   \"definition\":\"popin\"}}">>),
%%    W, true =:= true.



