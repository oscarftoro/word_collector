%%%-------------------------------------------------------------------
%%% @author oscar
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Feb 2015 20:33
%%%-------------------------------------------------------------------
-module(wc_backend_tests).
-author("oscar").
-include("../include/diccionario.hrl").
-include_lib("eunit/include/eunit.hrl").

add_word_test() ->
        Word = #wc_word{title = "hund",language = "dk",definition = "perro",status = active,priority = low,
                examples = "min hund spiser din kat", locations = undefined, photos = undefined},

        ?assert(wc_backend:add_word(Word) =:= ok).

get_all_words_test()->
        Result =  wc_backend:get_all_words(),
        ?assert(length(Result) /= []).
