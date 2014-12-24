%%%-------------------------------------------------------------------
%%% @author oscar toro
%%% @copyright (CC) 2014,  Oscar Toro
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2014 13:58
%%%-------------------------------------------------------------------
-module(wc_action_handler).
-author("oscar").

-include_lib("eunit/include/eunit.hrl").
%% API
-export([init/3]).
-export([content_types_provided/2]).
-export([get_html/2]).

init(_, _Req, _Opts) ->
        {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
        {[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State) ->
        {<<"<html><body>This is REST!</body></html>">>, Req, State}.

create_word_test()->
        ok.