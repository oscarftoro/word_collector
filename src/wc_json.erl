-module(wc_json).
-export([encode/1,decode/1,record_to_proplist/1]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/diccionario.hrl").

-ifdef(debug_flag).
-define(DEBUG(X),io:format("DEBUG ~p: ~p ~p~n",[?MODULE,?LINE,X])).
-else.
-define(DEBUG(X),void).
-endif.


%% Encode Word or Language records to JSON
%%
-spec encode(#wc_word{} | [#wc_word{}]) -> binary().
encode(#wc_word{} = Rec) ->  
  jiffy:encode({record_to_proplist(Rec)});

  
encode(#wc_language{} = Rec) ->
  jiffy:encode({record_to_proplist(Rec)});

encode(L)->
  Words = [{record_to_proplist(W)}|| W <- L],
  jiffy:encode({[{words,Words}]}).
%% Decode Json Binaries to wc_words or wc_languages
%% 
-spec decode(binary()) ->#wc_word{} | #wc_language{}.
decode(Bin) ->
  {PrpList} = jiffy:decode(Bin),
  PropList  = [{binary_to_atom(K,utf8),V} || {K,V} <- PrpList],
  case hd(PropList) of
    {title,_Value}  -> decoder(PropList,lists:seq(2,11),#wc_word{});
    {name, _Value}  -> decoder(PropList,lists:seq(2,4),#wc_language{})
  end.

%% Auxiliar funtion that performs decoding of PropList.
%% Takes a Properlist with key/values of the record(PL)
%% A List of numbers with indexes of the record(L) 
%% And the record itself. Returns a record.
%% The Index 1 of a record is the record's name,
%% that is why the Indexes starts at 2, which is the first
%% value of the record.	  
decoder(PL,L, Rec) ->

  Values    = [V ||  {_K,V} <- PL], % all the values
  Formated  = lists:zip(L,Values), % proplist{int,binary() | []}
  lists:foldr(fun({K,V},Acc) -> setelement(K,Acc,V) end,Rec,Formated).   
    
-spec record_to_proplist(#wc_word{}) -> [{atom(),any()}].
record_to_proplist(#wc_word{} = Rec) ->
  lists:zip(record_info(fields,wc_word),   
  tl(tuple_to_list(Rec)));

record_to_proplist(#wc_language{} = Rec) ->
  lists:zip(record_info(fields,wc_language),
  tl(tuple_to_list(Rec))).

%%%%%%%%%%%%%%%%%%%
%%%%%%%TEST%%%%%%%%
%%%%%%%%%%%%%%%%%%%
encode_decode_word_test() ->

  W = #wc_word{title = <<"numse">>, definition = <<"popin">>},
  Encoded = encode(W),
  W2 = decode(Encoded),
  ?assert(<<"numse">> =:= W2#wc_word.title),     
  ?assert(<<"popin">> =:= W2#wc_word.definition).
    
encode_decode_language_test() ->
  L       = #wc_language{name = <<"español"/utf8>>, initials = <<"es">>, is_mother_language = true},
  L2      = #wc_language{name = <<"spanish"/utf8>>, initials = <<"sp">>, is_mother_language = true},
  Encoded = encode(L),
  Decoded = decode(Encoded), 

  Encoded2 = encode(L2),
  Decoded2 = decode(Encoded2),
  
  ?assert(<<"español"/utf8>> =:= Decoded#wc_language.name),
  ?assert(<<"spanish">>      =:= Decoded2#wc_language.name).




