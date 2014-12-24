%%%-------------------------------------------------------------------
%%% @author oscar
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2014 14:02
%%%-------------------------------------------------------------------
-module(wc_word_action_handler).
-author("oscar").

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
